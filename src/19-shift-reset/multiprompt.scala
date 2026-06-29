
// Multi-prompt delimited continuations.
//
// The original shift/reset interpreter gets its delimiting power from a trick
// that only works for ONE prompt: `Reset(e)` runs `e` under a *fresh identity*
// continuation and re-applies the outer `k` afterwards, while `Shift` discards
// the current continuation by running its body under that same identity. There
// is exactly one delimiter, so "the nearest reset" is unambiguous and a single
// host-level continuation suffices.
//
// That collapses as soon as there are several prompts: a `shift` on prompt `p`
// must capture the continuation up to the *dynamically nearest reset labelled
// p*, skipping over any intervening resets labelled with other prompts. A
// single opaque `Value => Value` cannot be sliced at an arbitrary prompt, so we
// reify the context as an inspectable data structure: a META-CONTINUATION, i.e.
// a stack of frames, each frame being either a captured continuation segment or
// a prompt marker.
//
// The primitives below are the four of Dybvig, Peyton Jones & Sabry, "A Monadic
// Framework for Delimited Continuations" (2007): newPrompt, pushPrompt,
// withSubCont, pushSubCont. They are the canonical multi-prompt core; classic
// shift / reset / control are then *derived* as macros at the bottom of the
// file, so the operators from the single-prompt interpreter reappear, now
// indexed by a prompt.

enum Exp:
  case Num(n: Int)
  case Id(name: String)
  case Add(lhs: Exp, rhs: Exp)
  case Fun(param: String, body: Exp)
  case Ap(funExpr: Exp, argExpr: Exp)

  // --- multi-prompt primitives ---
  case NewPrompt                                  // generate a fresh, first-class prompt
  case PushPrompt(prompt: Exp, body: Exp)         // delimit `body` with the prompt
  case WithSubCont(prompt: Exp, f: Exp)           // capture the sub-continuation up to `prompt`
  case PushSubCont(subCont: Exp, body: Exp)       // reinstate a captured sub-continuation

object Exp:
  implicit def num2exp(n: Int): Exp = Num(n)
  implicit def id2exp(s: String): Exp = Id(s)

import Exp.*

// ---------------------------------------------------------------------------
// Values
// ---------------------------------------------------------------------------
sealed abstract class Value
type Env = Map[String, Value]
case class NumV(n: Int) extends Value
case class ClosureV(f: Fun, env: Env) extends Value
case class PromptV(id: Int) extends Value          // a first-class prompt
case class SubContV(frames: MetaCont) extends Value // a reified slice of the meta-continuation

// ---------------------------------------------------------------------------
// Continuations and the meta-continuation
// ---------------------------------------------------------------------------
// A `Cont` is one *segment* of context: a piece of pure continuation that lives
// within a single prompt region. It threads the meta-continuation so that when
// the segment falls off its bottom, control flows into the enclosing frames.
type Cont = (Value, MetaCont) => Value

// The meta-continuation is a stack of frames; the head is the innermost one.
enum Frame:
  case Seg(k: Cont)      // a captured continuation segment
  case Prompt(id: Int)   // a delimiter installed by PushPrompt
type MetaCont = List[Frame]

import Frame.*

// Returning a value to the meta-continuation: pop frames until the value has a
// home. A prompt marker is transparent to a *returning* value (it is only a
// barrier to `withSubCont`), so we simply discard it and keep returning.
def applyMeta(mk: MetaCont, v: Value): Value = mk match
  case Nil               => v
  case Prompt(_) :: rest => applyMeta(rest, v)
  case Seg(k)    :: rest => k(v, rest)

// Slice the meta-continuation at the nearest `Prompt(p)`. The captured part is
// everything strictly above that prompt (the prompt itself is consumed, à la
// control0 / withSubCont); `rest` is everything below it.
def splitAt(p: Int, mk: MetaCont): (MetaCont, MetaCont) = mk match
  case Nil                          => sys.error(s"withSubCont: no enclosing prompt $p")
  case Prompt(q) :: rest if q == p  => (Nil, rest)
  case fr :: rest =>
    val (captured, below) = splitAt(p, rest)
    (fr :: captured, below)

// The base continuation of a delimited region: hand the result to whatever
// frames are currently below.
val ret: Cont = (v, mk) => applyMeta(mk, v)

// ---------------------------------------------------------------------------
// Fresh prompts
// ---------------------------------------------------------------------------
private var promptCounter = 0
def freshPrompt(): Int = { promptCounter += 1; promptCounter }

// ---------------------------------------------------------------------------
// Evaluator (CPS, threading both the current segment `k` and the meta-cont)
// ---------------------------------------------------------------------------
def eval(e: Exp, env: Env, k: Cont, mk: MetaCont): Value = e match
  case Num(n) => k(NumV(n), mk)
  case Id(x)  => k(env(x), mk)

  case Add(l, r) =>
    eval(l, env, (lv, mkl) => {
      eval(r, env, (rv, mkr) => {
        (lv, rv) match
          case (NumV(a), NumV(b)) => k(NumV(a + b), mkr)
          case _ => sys.error("can only add numbers")
      }, mkl)
    }, mk)

  case f @ Fun(_, _) => k(ClosureV(f, env), mk)

  case Ap(f, a) =>
    eval(f, env, (clv, mkf) => {
      eval(a, env, (av, mka) => {
        clv match
          case ClosureV(fn, cenv) => eval(fn.body, cenv + (fn.param -> av), k, mka)
          case _ => sys.error("can only apply functions")
      }, mkf)
    }, mk)

  // Generate a fresh prompt and continue.
  case NewPrompt => k(PromptV(freshPrompt()), mk)

  // Install a prompt: push the marker and the current continuation, then run
  // `body` from a clean base. When `body` returns, `ret` walks back out through
  // the prompt (popping it) into `Seg(k)`, i.e. resumes the original `k`.
  case PushPrompt(pe, body) =>
    eval(pe, env, (pv, mkp) => {
      pv match
        case PromptV(p) => eval(body, env, ret, Prompt(p) :: Seg(k) :: mkp)
        case _ => sys.error("pushPrompt: not a prompt")
    }, mk)

  // Capture the sub-continuation up to the nearest `Prompt(p)` (the current
  // segment `k` is part of it), reify it as a SubContV, and apply `f` to it
  // *below* that prompt — the delimiter is consumed by the capture.
  case WithSubCont(pe, fe) =>
    eval(pe, env, (pv, mkp) => {
      pv match
        case PromptV(p) =>
          eval(fe, env, (fv, mkf) => {
            val (captured, below) = splitAt(p, Seg(k) :: mkf)
            fv match
              case ClosureV(fn, cenv) =>
                eval(fn.body, cenv + (fn.param -> SubContV(captured)), ret, below)
              case _ => sys.error("withSubCont: not a function")
          }, mkp)
        case _ => sys.error("withSubCont: not a prompt")
    }, mk)

  // Reinstate a captured sub-continuation on top of the current context, then
  // run `body`; `body`'s result flows up through the reinstated frames.
  case PushSubCont(se, body) =>
    eval(se, env, (sv, mks) => {
      sv match
        case SubContV(frames) => eval(body, env, ret, frames ++ (Seg(k) :: mks))
        case _ => sys.error("pushSubCont: not a sub-continuation")
    }, mk)

def run(e: Exp): Value = eval(e, Map.empty, ret, Nil)

// ---------------------------------------------------------------------------
// Derived classic operators (macros over the primitives)
// ---------------------------------------------------------------------------
// These rebuild reset / shift / control on top of the four primitives, exactly
// as in the DPS paper. `prompt` should be a variable (an Id) since the macro
// duplicates it. With one prompt these collapse to the original shift/reset.
private var g = 0
private def gensym(s: String): String = { g += 1; s"$s%$g" }

def let(name: String, rhs: Exp, body: Exp): Exp = Ap(Fun(name, body), rhs)

//   reset p e  ==  pushPrompt p e
def reset(prompt: Exp, body: Exp): Exp = PushPrompt(prompt, body)

//   shift p (k. e)  ==
//     withSubCont p (s. pushPrompt p (let k = (x. pushPrompt p (pushSubCont s x)) in e))
// The captured continuation re-installs prompt p, so it is delimited/composable.
def shift(prompt: Exp, k: String, body: Exp): Exp =
  val s = gensym("s"); val x = gensym("x")
  val capturedAsFun = Fun(x, PushPrompt(prompt, PushSubCont(Id(s), Id(x))))
  WithSubCont(prompt, Fun(s, PushPrompt(prompt, let(k, capturedAsFun, body))))

//   control p (k. e)  ==
//     withSubCont p (s. pushPrompt p (let k = (x. pushSubCont s x) in e))
// Here the captured continuation does NOT re-install a prompt, so it composes
// flatly (Felleisen's F operator).
def control(prompt: Exp, k: String, body: Exp): Exp =
  val s = gensym("s"); val x = gensym("x")
  val capturedAsFun = Fun(x, PushSubCont(Id(s), Id(x)))
  WithSubCont(prompt, Fun(s, PushPrompt(prompt, let(k, capturedAsFun, body))))

// ---------------------------------------------------------------------------
// Examples
// ---------------------------------------------------------------------------
def numOf(v: Value): Int = v match { case NumV(n) => n; case _ => sys.error("not a number") }
def check(name: String, e: Exp, expect: Int): Unit =
  val got = numOf(run(e))
  println(s"[${if got == expect then "OK  " else "FAIL"}] $name = $got (expect $expect)")

// reset p (1 + shift p (k. k (k 10)))          -- compose the captured cont twice
check("shift compose",
  let("p", NewPrompt,
    reset("p", Add(1, shift("p", "k", Ap("k", Ap("k", 10)))))),
  12)

// reset p (10 + shift p (k. 100))              -- discard the continuation
check("shift discard",
  let("p", NewPrompt, reset("p", Add(10, shift("p", "k", 100)))),
  100)

// reset p ( reset q ( shift p (k. 1) + 10 ) + 100 )
//   shift p escapes the inner q-delimiter; result is 1.
check("escape inner prompt",
  let("p", NewPrompt, let("q", NewPrompt,
    reset("p", Add(reset("q", Add(shift("p", "k", 1), 10)), 100)))),
  1)

// reset p ( reset q ( shift q (k. k 5) + 10 ) + 100 )  -- handled locally: (5+10)+100
check("local prompt",
  let("p", NewPrompt, let("q", NewPrompt,
    reset("p", Add(reset("q", Add(shift("q", "k", Ap("k", 5)), 10)), 100)))),
  115)

// reset p (1 + reset q (10 + shift q (k. k (k 100))))  -- 1 + (((100+10)+10))
check("nested compose",
  let("p", NewPrompt, let("q", NewPrompt,
    reset("p", Add(1, reset("q", Add(10, shift("q", "k", Ap("k", Ap("k", 100))))))))),
  121)

// reset p (1 + control p (f. 10 + f 100))  -- control composes flatly: 1 + (10 + 100)
check("control flat",
  let("p", NewPrompt, reset("p", Add(1, control("p", "f", Add(10, Ap("f", 100)))))),
  111)
