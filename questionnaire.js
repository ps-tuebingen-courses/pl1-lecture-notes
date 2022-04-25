"use strict";
// #### QUESTIONNAIRE MODULE ####
// This module implements the questionnaire functionality
;
;
// ########### PARSE METHODS
function parseQuestionnaire(questionnaire) {
    const questions = Array.from(questionnaire.children);
    const lang = questionnaire.getAttribute('language');
    return {
        rootElement: questionnaire,
        questions: questions.map(x => parseQuestion(x)),
        language: lang == 'de' || lang == 'en' ? lang : 'en'
    };
}
function parseQuestion(question) {
    const type = question.getAttribute('type');
    const solution_count = question.getElementsByTagName("solution").length;
    const text = Array.from(question.childNodes)
        .filter(x => x.tagName != 'DISTRACTOR'
        && x.tagName != 'SOLUTION');
    const answers = Array.from(question.childNodes)
        .filter(x => x.tagName == 'DISTRACTOR'
        || x.tagName == 'SOLUTION');
    return {
        type: getQuestionType(type, solution_count),
        text: text,
        answers: answers.map(x => parseAnswer(x)),
        solutionCount: solution_count,
        rootElement: question
    };
}
// String, Number, HTMLElement -> String | DOM Manipulation
// optional singlechoice / multiplechoice attribute
// automatically assigns the correct type, if not given single- or multiplechoice
// if optional attribute is assigned throw an error for invalid arguments
function getQuestionType(type, solution_count) {
    if (solution_count == 1) {
        console.log("inferred type: singlechoice");
        return "singlechoice";
    }
    else if (solution_count > 1) {
        console.log("inferred type: multiplechoice");
        return "multiplechoice";
    }
}
function parseAnswer(answer) {
    const correct = (answer.tagName == 'SOLUTION') ? true : false;
    const text = Array.from(answer.childNodes)
        .filter(x => x.tagName != 'EXPLANATION');
    const explanation = answer.getElementsByTagName('explanation')[0];
    return {
        correct: correct,
        text: text,
        explanation: explanation,
        rootElement: answer
    };
}
// ############### main function
function setup() {
    // setup style
    const styleNode = document.createElement('style');
    styleNode.innerHTML = Ressources.style;
    document.getElementsByTagName('head')[0].appendChild(styleNode);
    const q_col = document.getElementsByTagName("questionnaire");
    // render every questionnaire in the HTML Document
    for (let i = q_col.length - 1; i >= 0; i--) {
        const questionnaire = q_col[i];
        // validate htmL Structure before parsing
        if (validateQuestionnaireStructure(questionnaire) == true) {
            const r = parseQuestionnaire(questionnaire);
            console.log(r);
            if (validateQuesionnaireAttributes(r) == true) {
                // Possible ValidationPoint (Attributes)
                renderQuestionnaire(r);
            }
            else {
                console.log('invalid questionnaire attributes');
            }
        }
        else {
            console.log('invalid questionnaire structure');
            console.log(validateQuestionnaireStructure(questionnaire));
        }
    }
}
window.onload = setup;
// render questionnaire:
// addEventListener for "click"-Events
// build wrapper-<div> and <img>-icons for
// - answer
// - question
// ### RENDER FUNCTIONS ###
// - state is stored in the DOM via attributes
// - as much hiding/showing as possible is done via css classes depending
//   on those attributes
// store initial State after rendering for re-access
const renderedQuestionnaires = [];
const i18n = {
    'en': {
        goto: 'Go to question',
        select: 'Select answer',
        toggle: 'Show/hide explanation',
        wrong: 'Wrong!',
        correct: 'Correct!',
        next: 'Next',
        submit: 'Submit',
        reset: 'Reset',
        feedbacks: ['Keep trying!', 'Okay', 'Better Luck next time!',
            'Not bad!', 'Great!', 'Perfect!']
    },
    'de': {
        goto: 'Gehe zu Frage',
        select: 'Wähle die Antwort aus',
        toggle: 'Verstecke/Zeige Erklärung',
        wrong: 'Falsch!',
        correct: 'Richtig!',
        next: 'Weiter',
        submit: 'Antwort prüfen',
        reset: 'Zurücksetzen',
        feedbacks: ['Nicht aufgeben!', 'Okay', 'Mehr Glück beim nächsten Mal!',
            'Nicht schlecht!', 'Super!', 'Perfekt!']
    }
};
let lang = 'en';
// ### Questionnaire
// <questionnaire>
//  - total_questions
//  - current_question
//  - render_id
function renderQuestionnaire(questionnaire) {
    // setup internationalization
    lang = questionnaire.language;
    // setup questionnaire properties
    const root = questionnaire.rootElement;
    root.setAttribute("total_questions", "" + questionnaire.questions.length);
    root.setAttribute("current_question", "1");
    root.setAttribute("language", lang);
    // render HTML
    root.innerHTML = `
    <div class="content-wrapper">
      <div class="question-overview">
      ${questionnaire.questions.map((_, i) => `
        <p   class="bubble bubble-pending ${i == 0 ? 'bubble-current' : ''}"
             question="${i + 1}"
             onclick="gotoQuestion(event)"
             title="${i18n[lang].goto} ${i + 1}"></p>`).join('')}
      </div>
      ${questionnaire.questions.map(renderQuestion).join('')}
      ${questionnaire.questions.length <= 1 ? '' : `
      <div class="summary">
        <div class="summary-bar-container">
          <div class="summary-bar"></div>
        </div>
        <p class="summary-text"></p>
        <button onclick="resetQuestionnaire(event)"
                class="reset-button">
          ${i18n[lang].reset}
        </button>
      </div>
      `}
    </div>
  `;
    // store rendered innerHTML for resets
    root.setAttribute('render_id', (renderedQuestionnaires.push(root.innerHTML) - 1).toString());
}
function gotoQuestion(e) {
    const el = e.target;
    const navTarget = el.getAttribute('question');
    // set current question
    const questionnaire = getTagRecursive(el, 'questionnaire');
    questionnaire.setAttribute('current_question', navTarget);
    // set question visible
    const questions = questionnaire.getElementsByTagName('question');
    Array.from(questions).map(q => {
        if (q.getAttribute('number') == navTarget) {
            q.setAttribute('visible', 'true');
        }
        else {
            q.removeAttribute('visible');
        }
    });
    // hide summary
    const summary = questionnaire.getElementsByClassName('summary')[0];
    summary.removeAttribute('visible');
    // set bubble class
    const bubbles = questionnaire.getElementsByClassName('bubble');
    Array.from(bubbles).map(b => {
        if (b.getAttribute('question') == navTarget) {
            b.classList.add('bubble-current');
        }
        else {
            b.classList.remove('bubble-current');
        }
    });
}
function resetQuestionnaire(e) {
    const el = e.target;
    const questionnaire = getTagRecursive(el, 'questionnaire');
    questionnaire.setAttribute('current_question', '1');
    questionnaire.innerHTML = renderedQuestionnaires[parseInt(questionnaire.getAttribute('render_id'))];
}
// ### Question
// <question>
//  - type: "singlechoice"|"multiplechoice"
//  - visible: "true" or not present
//  - answer: "pending"|"correct"|"wrong"
function renderQuestion(question, index) {
    return `
    <question type="${question.type}"
              ${index == 0 ? 'visible="true"' : ''}
              number="${index + 1}"
              answer="pending">
      <div class="correct-text">
        <p>${i18n[lang].correct}</p>
      </div>
      <div class="wrong-text">
        <p>${i18n[lang].wrong}</p>
      </div>
      <div class="question-header">
        <div>${question.text.map(nodeOuterHTML).join('')}</div>
      </div>
      ${question.answers.map((x) => renderAnswer(question.type, x)).join('')}
      <div class="question-footer">
        <div class="next-button" onClick="showNextQuestion(event)">
          ${i18n[lang].next}
        </div>
        <div class="submit-button" onClick="submitAnswer(event)">
          ${i18n[lang].submit}
        </div>
      </div>
    </question>
  `;
}
// event handlers:
// continue
function showNextQuestion(event) {
    var _a;
    const el = event.target;
    const currentQuestion = getTagRecursive(el, "question");
    const questionnaire = getTagRecursive(currentQuestion, "questionnaire");
    // update header
    const total_questions = parseInt(questionnaire.getAttribute("total_questions"));
    const current_question = parseInt(questionnaire.getAttribute("current_question"));
    if (current_question == total_questions) {
        // update questionnaire
        questionnaire.setAttribute('current_question', '0');
        // ### compute summary
        const questions = Array.from(questionnaire.getElementsByTagName('question'));
        const correct = questions.filter(q => q.getAttribute('answer') == 'correct').length;
        const ratio = (correct / questions.length);
        const percentage = (ratio * 100).toPrecision(3);
        // adjust bar length
        const summaryBar = questionnaire.getElementsByClassName('summary-bar')[0];
        summaryBar.innerHTML = '?';
        summaryBar.style.width = `${percentage}%`;
        summaryBar.animate([
            { width: 0 },
            { width: `${percentage}%`, easing: 'ease-out' }
        ], 1000);
        // show text after animation
        window.setTimeout(() => {
            summaryBar.innerHTML = `${percentage}%`;
            // adjust text
            const lang = questionnaire.getAttribute('language');
            const feedbacks = i18n[lang].feedbacks;
            const summaryText = questionnaire.getElementsByClassName('summary-text')[0];
            summaryText.innerHTML = feedbacks[Math.floor(ratio * 0.99 * feedbacks.length)];
        }, 1000);
    }
    else {
        // update questionnaire
        questionnaire.setAttribute("current_question", (current_question + 1).toString());
    }
    // update header
    const bubbles = questionnaire.getElementsByClassName('bubble');
    Array.from(bubbles).map(b => {
        if (b.getAttribute('question') == '' + (current_question + 1)) {
            b.classList.add('bubble-current');
        }
        else {
            b.classList.remove('bubble-current');
        }
    });
    // update visibility
    (_a = currentQuestion.nextElementSibling) === null || _a === void 0 ? void 0 : _a.setAttribute('visible', 'true');
    currentQuestion.removeAttribute('visible');
}
// submit
function submitAnswer(event) {
    const el = event.target;
    const question = getTagRecursive(el, "question");
    const answers = question.getElementsByTagName('answer');
    // check correctness
    const correct = Array.from(answers).every(a => a.getAttribute('correct') == a.getAttribute('selected'));
    // update question
    question.setAttribute('answer', correct ? 'correct' : 'wrong');
    // update bubble
    const questionnaire = getTagRecursive(question, 'questionnaire');
    const bubbles = questionnaire.getElementsByClassName('bubble');
    Array.from(bubbles).filter(b => b.getAttribute('question') == question.getAttribute('number')).map(b => {
        b.classList.remove('bubble-pending');
        if (correct) {
            b.classList.add('bubble-correct');
        }
        else {
            b.classList.add('bubble-wrong');
        }
    });
    // expand necessary explanations, if present
    Array.from(answers).map(a => {
        if (a.getAttribute('selected') != a.getAttribute('correct')) {
            a.setAttribute('expanded', 'true');
        }
    });
}
// ### Answer
// <answer>
//  - correct: "true"|"false"
//  - selected: "true"|"false"
function renderAnswer(type, answer) {
    return `
  <answer correct="${answer.correct ? 'true' : 'false'}"
          selected="false"
          expanded="false">
    <div class="wrapper-answer"
         onclick="selectAnswer(event)"
         title="${i18n[lang].select}">
      <img class="answer-mark" src="${type == 'singlechoice' ? Ressources.circle_regular : Ressources.square_regular}">
      <img class="answer-mark answer-mark-selected"
           src="${Ressources.xmark_solid}">
      <div class="answer-text-container">
        ${answer.text.map(nodeOuterHTML).join('')}
      </div>
      ${answer.explanation == undefined ? '' : `
        <img class="expander"
             title="${i18n[lang].toggle}"
             src="${Ressources.angle_down_solid}">
        <img class="collapser"
             title="${i18n[lang].toggle}"
             src="${Ressources.angle_up_solid}">
      `}
    </div>
    ${(answer.explanation == undefined) ? '' : answer.explanation.outerHTML}
  </answer>
  `;
}
// helper
function nodeOuterHTML(x) {
    const outerHTML = x.outerHTML;
    if (outerHTML == undefined) {
        const data = x.data;
        if (data == undefined) {
            return '';
        }
        return data;
    }
    console.log("outerHTML:" + outerHTML);
    return outerHTML;
}
// event handler
function selectAnswer(event) {
    const el = event.target;
    const answer = getTagRecursive(el, 'answer');
    const question = getTagRecursive(answer, 'question');
    if (question.getAttribute('answer') == 'pending') {
        // toggle answer selection
        if (answer.getAttribute('selected') == 'false') {
            answer.setAttribute('selected', 'true');
            if (question.getAttribute('type') == 'singlechoice') {
                submitAnswer(event);
            }
        }
        else {
            answer.setAttribute('selected', 'false');
        }
    }
    else {
        // toggle showing of explanation
        if (answer.getAttribute('expanded') == 'true') {
            answer.setAttribute('expanded', 'false');
        }
        else {
            answer.setAttribute('expanded', 'true');
        }
    }
}
// ### Error
// render Function
function renderError(current_el, error, message) {
    const questionnaire = getTagRecursive(current_el, "questionnaire");
    const error_html = `
  <div class="error-wrapper">
    <div class="error-header">
      <h2> Why do I see this red funny box?</h2>
    </div>
    <div class="error-box">
    <p>${error}</p>
    </div>
    <pre>
    <code class="error-message">
    ${escapeHtml(message)}
    </code>
    </pre>
  </div>
  `;
    questionnaire.innerHTML = error_html;
    console.log(error_html);
}
// helper
// escape HTML TAGS
function escapeHtml(str) {
    return str.replace(/&/g, "&amp;")
        .replace(/</g, "&lt;")
        .replace(/>/g, "&gt;")
        .replace(/"/g, "&quot;")
        .replace(/'/g, "&#39;");
}
// ######### VALIDATION METHODS ##########
// validateQuestionnaireStructure
// checks if all necessary tags in questionnaire have the correct parentElement
function validateQuestionnaireStructure(questionnaire) {
    let questions = questionnaire.getElementsByTagName("question");
    let answers = questionnaire.getElementsByTagName("answer");
    let explanation = questionnaire.getElementsByTagName("explanation");
    // validate given html tag elements
    return Array.from(questions).every(q => validateStructure(q)) &&
        Array.from(answers).every(a => validateStructure(a)) &&
        Array.from(explanation).every(e => validateStructure(e));
}
// ValidateStructure
// <questionnaire> -> <question> -> at least 2 <answer> -> <explanation>
// return either messageString or Boolean: True
function validateStructure(el) {
    const html_tag = el.tagName;
    const parent = el.parentElement;
    switch (html_tag) {
        case 'QUESTION':
            return parentHasToBe(el, parent, "QUESTIONNAIRE");
        case 'SOLUTION':
        case 'DISTRACTOR':
            return parentHasToBe(el, parent, "QUESTION");
        case 'EXPLANATION':
            return parentHasToBe(el, parent, "SOLUTION", "DISTRACTOR");
        default:
            console.log('html_tag to check was no question, solution, distractor or explanation');
            return true;
    }
}
function parentHasToBe(el, parent, tag, tag_two) {
    if ((parent === null || parent === void 0 ? void 0 : parent.tagName) == tag || (parent === null || parent === void 0 ? void 0 : parent.tagName) == tag_two) {
        return true;
    }
    else {
        let err = `HTML structure is invalid: Please check your input at: `;
        let msg = parent === null || parent === void 0 ? void 0 : parent.outerHTML;
        renderError(el, err, msg);
        return false;
    }
}
function validateQuesionnaireAttributes(questionnaire) {
    const questions = questionnaire.questions;
    for (let i = 0; i < questions.length; i++) {
        if (validateQuestionAttributes(questions[i]) == false) {
            return false;
        }
    }
    return true;
}
function validateQuestionAttributes(question) {
    const type = question.type;
    const answers = question.answers.length;
    const solutions = question.solutionCount;
    // if only 1 or less answers exists
    if (answers < 2) {
        let err = `You need to provide at least two answers for one &lt;question&gt;:`;
        let msg = question.rootElement.outerHTML;
        renderError(question.rootElement, err, msg);
        return false;
    }
    // if there is no solution
    else if (solutions == 0) {
        let err = `This question has no &lt;solution&gt;:`;
        let msg = question.rootElement.outerHTML;
        renderError(question.rootElement, err, msg);
        return false;
    }
    return true;
}
// ######## HELPER FUNCTIONS #######
// getTagRecursive from a child element
// if element has TagName
// return;
// else: retry with parentElement
function getTagRecursive(el, tag) {
    if (el.tagName == tag.toUpperCase()) {
        return el;
    }
    else {
        let parent = el.parentElement;
        let result = getTagRecursive(parent, tag);
        return result;
    }
}
// ### EVENT HANDLER FUNCTIONS ###
// EVENT AFTER BUTTON "prev" OR "next" CLICK
// questionChangeHandler
// EventHandler -> DOM Manipulation
// function questionChangeHandler(event: Event) {
//   // get Questionnaire attributes
//   let el: HTMLElement = event.target as HTMLElement;
//   let button = el.getAttribute("id");
//   let questionnaire: HTMLElement = getTagRecursive(el, "questionnaire") as HTMLElement;
//   let total_questions: number = parseInt(questionnaire.getAttribute("total_questions") as string);
//   let current_question: number = parseInt(questionnaire.getAttribute("current_question") as string);
//   let questions = questionnaire.getElementsByTagName("question");
//   let min_qid: number = 0;
//   let max_qid: number = total_questions - 1;
//   let qid = current_question - 1;
//   // change question
//   // if button is "prev"
//   switch (button) {
//     case "prev_button":
//       let prev_qid = current_question - 1;
//       // change visibility to the previous question
//       questions[qid].removeAttribute("visible");
//       questions[qid - 1].setAttribute("visible", "true");
//       // change question overview text
//       questionnaire.setAttribute("current_question", prev_qid.toString());
//       questionnaire.getElementsByClassName("question-overview")[0].textContent = "Question " + prev_qid.toString() + " of " + total_questions;
//       //hide button if button to first question
//       if (prev_qid - 1 == min_qid) {
//         el.setAttribute("style", "visibility:hidden;");
//       }
//       // show next-Button
//       el.nextElementSibling ?.setAttribute("style", "visibility:visible;");
//       break;
//
//     case "next_button":
//       let next_qid = qid + 1;
//       questions[qid].removeAttribute("visible");
//       questions[next_qid].setAttribute("visible", "true");
//       //change question overview text
//       questionnaire.setAttribute("current_question", (current_question + 1).toString());
//       questionnaire.getElementsByClassName("question-overview")[0].textContent = "Question " + (current_question + 1).toString() + " of " + total_questions;
//       // hide button if button to last question
//       if (next_qid == max_qid) {
//         el.setAttribute("style", "visibility:hidden;");
//       }
//       //show prev_button
//       el.previousElementSibling ?.setAttribute("style", "visibility:visible;");
//       break;
//
//     default:
//       console.log("No Button caused this EventHandler", button);
//       break;
//   }
//
//
// }
//
//
//
// // CollapseEventHandler
// // Handles Collapse Event for shoowing explanation texts
// function collapseEventHandler(event: Event) {
//   const el = event.target as HTMLElement;
//   const question: HTMLElement = getTagRecursive(el, "question") as HTMLElement;
//   const answers: HTMLCollection = question.getElementsByTagName("answer") as HTMLCollection;
//   //change icons and collapse
//   if (el.getAttribute("clicked") == "true") {
//     el.setAttribute("src", Ressources.plus_solid);
//     el.setAttribute("clicked", "false");
//     for (let i = answers.length - 1; i >= 0; i--) {
//       let answer = answers[i] as HTMLElement;
//       let explanation = answer.getElementsByTagName("explanation")[0];
//       (explanation != undefined) ? explanation.removeAttribute("visible"): "";
//     }
//   }
//   else {
//     el.setAttribute("src", Ressources.minus_solid);
//     el.setAttribute("clicked", "true");
//     for (let i = answers.length - 1; i >= 0; i--) {
//       let answer = answers[i] as HTMLElement;
//       let explanation = answer.getElementsByTagName("explanation")[0];
//       (explanation != undefined) ? explanation.setAttribute("visible", "true"): "";
//     }
//   }
// }
// // show <explanation>
// //Answer->DOM Manipulation
// function showExplanation(answer: HTMLElement) {
//   let explanation: HTMLElement = answer.getElementsByTagName("explanation")[0] as HTMLElement;
//   if (explanation.getAttribute("visible") == "true") {
//     explanation.removeAttribute("visible");
//   }
//   else {
//     explanation.setAttribute("visible", "true");
//   }
// }
// // unified click on answer event handler
// function clickAnswerHandler(event: Event) {
//   const el = event.target as HTMLElement;
//   checkAnswerEventHandler(el);
//   // show Explanation
//   let answer = getTagRecursive(el, "answer");
//   if (answer.getElementsByTagName('explanation').length == 0){
//     console.log("There is no explanation");
//   }
//   else{
//     showExplanation(answer);
//   }
// }
//
//
// // check click for correct answer
// // depending on question type show either
// // - for multiplechoice just clicked answer
// // - for singlechoice all answers
// function checkAnswerEventHandler(el: HTMLElement) {
//   let question_type = getTagRecursive(el, "question").getAttribute("type");
//   if (question_type == "multiplechoice") {
//     let answer = getTagRecursive(el, "answer");
//     showAnswer(answer);
//   }
//   else if (question_type == "singlechoice") {
//     let answers = getTagRecursive(el, "question").getElementsByTagName("answer") as HTMLCollection;
//     for (let i = answers ?.length - 1; i >= 0; i--) {
//       let answer: HTMLElement = answers[i] as HTMLElement;
//       showAnswer(answer);
//     }
//   }
// }
// // showAnswer
// // show icons and highlight answer
// function showAnswer(answer: HTMLElement) {
//   answer.setAttribute("clicked", "true");
//   let img = answer.getElementsByTagName("img")[0];
//   if (answer.getAttribute("correct") == "true") {
//     img.setAttribute("src", Ressources.check_solid);
//   }
//   else {
//     img.setAttribute("src", Ressources.xmark_solid);
//   }
// }
var Ressources;
(function (Ressources) {
    Ressources.angle_down_solid = `data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCAzODQgNTEyIj48IS0tISBGb250IEF3ZXNvbWUgUHJvIDYuMS4xIGJ5IEBmb250YXdlc29tZSAtIGh0dHBzOi8vZm9udGF3ZXNvbWUuY29tIExpY2Vuc2UgLSBodHRwczovL2ZvbnRhd2Vzb21lLmNvbS9saWNlbnNlIChDb21tZXJjaWFsIExpY2Vuc2UpIENvcHlyaWdodCAyMDIyIEZvbnRpY29ucywgSW5jLiAtLT48cGF0aCBkPSJNMTkyIDM4NGMtOC4xODggMC0xNi4zOC0zLjEyNS0yMi42Mi05LjM3NWwtMTYwLTE2MGMtMTIuNS0xMi41LTEyLjUtMzIuNzUgMC00NS4yNXMzMi43NS0xMi41IDQ1LjI1IDBMMTkyIDMwNi44bDEzNy40LTEzNy40YzEyLjUtMTIuNSAzMi43NS0xMi41IDQ1LjI1IDBzMTIuNSAzMi43NSAwIDQ1LjI1bC0xNjAgMTYwQzIwOC40IDM4MC45IDIwMC4yIDM4NCAxOTIgMzg0eiIvPjwvc3ZnPg==`;
    Ressources.angle_left_solid = `data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCAyNTYgNTEyIj48IS0tISBGb250IEF3ZXNvbWUgUHJvIDYuMS4wIGJ5IEBmb250YXdlc29tZSAtIGh0dHBzOi8vZm9udGF3ZXNvbWUuY29tIExpY2Vuc2UgLSBodHRwczovL2ZvbnRhd2Vzb21lLmNvbS9saWNlbnNlIChDb21tZXJjaWFsIExpY2Vuc2UpIENvcHlyaWdodCAyMDIyIEZvbnRpY29ucywgSW5jLiAtLT48cGF0aCBkPSJNMTkyIDQ0OGMtOC4xODggMC0xNi4zOC0zLjEyNS0yMi42Mi05LjM3NWwtMTYwLTE2MGMtMTIuNS0xMi41LTEyLjUtMzIuNzUgMC00NS4yNWwxNjAtMTYwYzEyLjUtMTIuNSAzMi43NS0xMi41IDQ1LjI1IDBzMTIuNSAzMi43NSAwIDQ1LjI1TDc3LjI1IDI1NmwxMzcuNCAxMzcuNGMxMi41IDEyLjUgMTIuNSAzMi43NSAwIDQ1LjI1QzIwOC40IDQ0NC45IDIwMC4yIDQ0OCAxOTIgNDQ4eiIvPjwvc3ZnPg==`;
    Ressources.angle_right_solid = `data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCAyNTYgNTEyIj48IS0tISBGb250IEF3ZXNvbWUgUHJvIDYuMS4wIGJ5IEBmb250YXdlc29tZSAtIGh0dHBzOi8vZm9udGF3ZXNvbWUuY29tIExpY2Vuc2UgLSBodHRwczovL2ZvbnRhd2Vzb21lLmNvbS9saWNlbnNlIChDb21tZXJjaWFsIExpY2Vuc2UpIENvcHlyaWdodCAyMDIyIEZvbnRpY29ucywgSW5jLiAtLT48cGF0aCBkPSJNNjQgNDQ4Yy04LjE4OCAwLTE2LjM4LTMuMTI1LTIyLjYyLTkuMzc1Yy0xMi41LTEyLjUtMTIuNS0zMi43NSAwLTQ1LjI1TDE3OC44IDI1Nkw0MS4zOCAxMTguNmMtMTIuNS0xMi41LTEyLjUtMzIuNzUgMC00NS4yNXMzMi43NS0xMi41IDQ1LjI1IDBsMTYwIDE2MGMxMi41IDEyLjUgMTIuNSAzMi43NSAwIDQ1LjI1bC0xNjAgMTYwQzgwLjM4IDQ0NC45IDcyLjE5IDQ0OCA2NCA0NDh6Ii8+PC9zdmc+`;
    Ressources.angle_up_solid = `data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCAzODQgNTEyIj48IS0tISBGb250IEF3ZXNvbWUgUHJvIDYuMS4xIGJ5IEBmb250YXdlc29tZSAtIGh0dHBzOi8vZm9udGF3ZXNvbWUuY29tIExpY2Vuc2UgLSBodHRwczovL2ZvbnRhd2Vzb21lLmNvbS9saWNlbnNlIChDb21tZXJjaWFsIExpY2Vuc2UpIENvcHlyaWdodCAyMDIyIEZvbnRpY29ucywgSW5jLiAtLT48cGF0aCBkPSJNMzUyIDM1MmMtOC4xODggMC0xNi4zOC0zLjEyNS0yMi42Mi05LjM3NUwxOTIgMjA1LjNsLTEzNy40IDEzNy40Yy0xMi41IDEyLjUtMzIuNzUgMTIuNS00NS4yNSAwcy0xMi41LTMyLjc1IDAtNDUuMjVsMTYwLTE2MGMxMi41LTEyLjUgMzIuNzUtMTIuNSA0NS4yNSAwbDE2MCAxNjBjMTIuNSAxMi41IDEyLjUgMzIuNzUgMCA0NS4yNUMzNjguNCAzNDguOSAzNjAuMiAzNTIgMzUyIDM1MnoiLz48L3N2Zz4=`;
    Ressources.check_solid = `data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCA0NDggNTEyIj48IS0tISBGb250IEF3ZXNvbWUgUHJvIDYuMS4wIGJ5IEBmb250YXdlc29tZSAtIGh0dHBzOi8vZm9udGF3ZXNvbWUuY29tIExpY2Vuc2UgLSBodHRwczovL2ZvbnRhd2Vzb21lLmNvbS9saWNlbnNlIChDb21tZXJjaWFsIExpY2Vuc2UpIENvcHlyaWdodCAyMDIyIEZvbnRpY29ucywgSW5jLiAtLT48cGF0aCBkPSJNNDM4LjYgMTA1LjRDNDUxLjEgMTE3LjkgNDUxLjEgMTM4LjEgNDM4LjYgMTUwLjZMMTgyLjYgNDA2LjZDMTcwLjEgNDE5LjEgMTQ5LjkgNDE5LjEgMTM3LjQgNDA2LjZMOS4zNzIgMjc4LjZDLTMuMTI0IDI2Ni4xLTMuMTI0IDI0NS45IDkuMzcyIDIzMy40QzIxLjg3IDIyMC45IDQyLjEzIDIyMC45IDU0LjYzIDIzMy40TDE1OS4xIDMzOC43TDM5My40IDEwNS40QzQwNS45IDkyLjg4IDQyNi4xIDkyLjg4IDQzOC42IDEwNS40SDQzOC42eiIvPjwvc3ZnPg==`;
    Ressources.circle_regular = `data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCA1MTIgNTEyIj48IS0tISBGb250IEF3ZXNvbWUgUHJvIDYuMS4wIGJ5IEBmb250YXdlc29tZSAtIGh0dHBzOi8vZm9udGF3ZXNvbWUuY29tIExpY2Vuc2UgLSBodHRwczovL2ZvbnRhd2Vzb21lLmNvbS9saWNlbnNlIChDb21tZXJjaWFsIExpY2Vuc2UpIENvcHlyaWdodCAyMDIyIEZvbnRpY29ucywgSW5jLiAtLT48cGF0aCBkPSJNNTEyIDI1NkM1MTIgMzk3LjQgMzk3LjQgNTEyIDI1NiA1MTJDMTE0LjYgNTEyIDAgMzk3LjQgMCAyNTZDMCAxMTQuNiAxMTQuNiAwIDI1NiAwQzM5Ny40IDAgNTEyIDExNC42IDUxMiAyNTZ6TTI1NiA0OEMxNDEuMSA0OCA0OCAxNDEuMSA0OCAyNTZDNDggMzcwLjkgMTQxLjEgNDY0IDI1NiA0NjRDMzcwLjkgNDY0IDQ2NCAzNzAuOSA0NjQgMjU2QzQ2NCAxNDEuMSAzNzAuOSA0OCAyNTYgNDh6Ii8+PC9zdmc+`;
    Ressources.circle_solid = `data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCA1MTIgNTEyIj48IS0tISBGb250IEF3ZXNvbWUgUHJvIDYuMS4xIGJ5IEBmb250YXdlc29tZSAtIGh0dHBzOi8vZm9udGF3ZXNvbWUuY29tIExpY2Vuc2UgLSBodHRwczovL2ZvbnRhd2Vzb21lLmNvbS9saWNlbnNlIChDb21tZXJjaWFsIExpY2Vuc2UpIENvcHlyaWdodCAyMDIyIEZvbnRpY29ucywgSW5jLiAtLT48cGF0aCBkPSJNNTEyIDI1NkM1MTIgMzk3LjQgMzk3LjQgNTEyIDI1NiA1MTJDMTE0LjYgNTEyIDAgMzk3LjQgMCAyNTZDMCAxMTQuNiAxMTQuNiAwIDI1NiAwQzM5Ny40IDAgNTEyIDExNC42IDUxMiAyNTZ6Ii8+PC9zdmc+`;
    Ressources.minus_solid = `data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCA0NDggNTEyIj48IS0tISBGb250IEF3ZXNvbWUgUHJvIDYuMS4wIGJ5IEBmb250YXdlc29tZSAtIGh0dHBzOi8vZm9udGF3ZXNvbWUuY29tIExpY2Vuc2UgLSBodHRwczovL2ZvbnRhd2Vzb21lLmNvbS9saWNlbnNlIChDb21tZXJjaWFsIExpY2Vuc2UpIENvcHlyaWdodCAyMDIyIEZvbnRpY29ucywgSW5jLiAtLT48cGF0aCBkPSJNNDAwIDI4OGgtMzUyYy0xNy42OSAwLTMyLTE0LjMyLTMyLTMyLjAxczE0LjMxLTMxLjk5IDMyLTMxLjk5aDM1MmMxNy42OSAwIDMyIDE0LjMgMzIgMzEuOTlTNDE3LjcgMjg4IDQwMCAyODh6Ii8+PC9zdmc+`;
    Ressources.plus_solid = `data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCA0NDggNTEyIj48IS0tISBGb250IEF3ZXNvbWUgUHJvIDYuMS4wIGJ5IEBmb250YXdlc29tZSAtIGh0dHBzOi8vZm9udGF3ZXNvbWUuY29tIExpY2Vuc2UgLSBodHRwczovL2ZvbnRhd2Vzb21lLmNvbS9saWNlbnNlIChDb21tZXJjaWFsIExpY2Vuc2UpIENvcHlyaWdodCAyMDIyIEZvbnRpY29ucywgSW5jLiAtLT48cGF0aCBkPSJNNDMyIDI1NmMwIDE3LjY5LTE0LjMzIDMyLjAxLTMyIDMyLjAxSDI1NnYxNDRjMCAxNy42OS0xNC4zMyAzMS45OS0zMiAzMS45OXMtMzItMTQuMy0zMi0zMS45OXYtMTQ0SDQ4Yy0xNy42NyAwLTMyLTE0LjMyLTMyLTMyLjAxczE0LjMzLTMxLjk5IDMyLTMxLjk5SDE5MnYtMTQ0YzAtMTcuNjkgMTQuMzMtMzIuMDEgMzItMzIuMDFzMzIgMTQuMzIgMzIgMzIuMDF2MTQ0aDE0NEM0MTcuNyAyMjQgNDMyIDIzOC4zIDQzMiAyNTZ6Ii8+PC9zdmc+`;
    Ressources.square_regular = `data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCA0NDggNTEyIj48IS0tISBGb250IEF3ZXNvbWUgUHJvIDYuMS4xIGJ5IEBmb250YXdlc29tZSAtIGh0dHBzOi8vZm9udGF3ZXNvbWUuY29tIExpY2Vuc2UgLSBodHRwczovL2ZvbnRhd2Vzb21lLmNvbS9saWNlbnNlIChDb21tZXJjaWFsIExpY2Vuc2UpIENvcHlyaWdodCAyMDIyIEZvbnRpY29ucywgSW5jLiAtLT48cGF0aCBkPSJNMzg0IDMyQzQxOS4zIDMyIDQ0OCA2MC42NSA0NDggOTZWNDE2QzQ0OCA0NTEuMyA0MTkuMyA0ODAgMzg0IDQ4MEg2NEMyOC42NSA0ODAgMCA0NTEuMyAwIDQxNlY5NkMwIDYwLjY1IDI4LjY1IDMyIDY0IDMySDM4NHpNMzg0IDgwSDY0QzU1LjE2IDgwIDQ4IDg3LjE2IDQ4IDk2VjQxNkM0OCA0MjQuOCA1NS4xNiA0MzIgNjQgNDMySDM4NEMzOTIuOCA0MzIgNDAwIDQyNC44IDQwMCA0MTZWOTZDNDAwIDg3LjE2IDM5Mi44IDgwIDM4NCA4MHoiLz48L3N2Zz4=`;
    Ressources.xmark_solid = `data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCAzMjAgNTEyIj48IS0tISBGb250IEF3ZXNvbWUgUHJvIDYuMS4wIGJ5IEBmb250YXdlc29tZSAtIGh0dHBzOi8vZm9udGF3ZXNvbWUuY29tIExpY2Vuc2UgLSBodHRwczovL2ZvbnRhd2Vzb21lLmNvbS9saWNlbnNlIChDb21tZXJjaWFsIExpY2Vuc2UpIENvcHlyaWdodCAyMDIyIEZvbnRpY29ucywgSW5jLiAtLT48cGF0aCBkPSJNMzEwLjYgMzYxLjRjMTIuNSAxMi41IDEyLjUgMzIuNzUgMCA0NS4yNUMzMDQuNCA0MTIuOSAyOTYuMiA0MTYgMjg4IDQxNnMtMTYuMzgtMy4xMjUtMjIuNjItOS4zNzVMMTYwIDMwMS4zTDU0LjYzIDQwNi42QzQ4LjM4IDQxMi45IDQwLjE5IDQxNiAzMiA0MTZTMTUuNjMgNDEyLjkgOS4zNzUgNDA2LjZjLTEyLjUtMTIuNS0xMi41LTMyLjc1IDAtNDUuMjVsMTA1LjQtMTA1LjRMOS4zNzUgMTUwLjZjLTEyLjUtMTIuNS0xMi41LTMyLjc1IDAtNDUuMjVzMzIuNzUtMTIuNSA0NS4yNSAwTDE2MCAyMTAuOGwxMDUuNC0xMDUuNGMxMi41LTEyLjUgMzIuNzUtMTIuNSA0NS4yNSAwczEyLjUgMzIuNzUgMCA0NS4yNWwtMTA1LjQgMTA1LjRMMzEwLjYgMzYxLjR6Ii8+PC9zdmc+`;
    Ressources.style = `/* ERROR MESSAGE */
questionnaire .error-wrapper{
  display:block;
  max-width: 600px;
  border: 5px solid red;
   margin: 0 auto;
  padding:0 20px;
}
.error-header h2{
margin: 10px 0;
}
questionnaire .error-box{
font-size:16pt;
line-height:1.5em;
}

.error-message{
  font-family:monospace;
  font-size:12pt;
}

questionnaire pre code {
  display: block;
  background: none;
  white-space: pre;
  -webkit-overflow-scrolling: touch;
  overflow-x: scroll;
  max-width: 100%;
  min-width: 100px;
  padding: 0;
}

/* QUESTIONNAIRE */
/* Basic layout */
questionnaire {
  display: block;
  margin: 40px 0 100px ;
}
questionnaire .content-wrapper {
  display: flex;
  flex-wrap: wrap;
  flex-direction: column;
  margin: 10px;
  padding: 10px;
  justify-content: center;
}

questionnaire .question-overview{
margin: 0 auto 10px;
font-size:1.1em;
}
questionnaire question, questionnaire .summary {
  width: 90%;
  margin: 0 auto;
  font-size: 18pt;
  padding:4vw;
  background-color: #fcfcfc;
}

questionnaire .question-header, questionnaire .question-footer, questionnaire .wrapper-answer {
  display: inline-flex;
  width: 100%;
}

questionnaire .question-header {
  justify-content: space-between;
  margin-bottom: 0.5em;
}

questionnaire .question-footer{
  justify-content: center;
}

questionnaire .wrapper-answer, questionnaire explanation {
  border: 1px solid #eee;
  padding: 5px 12px;
  font-size: 14pt;
  margin: 15px 0 0;
  width:90%;
}
questionnaire explanation {
  margin-top: 0;
  border-top: 0;
}
questionnaire answer[correct="true"] explanation {
  border: 2px solid lightgreen;
  border-top: 0;
}
questionnaire answer[correct="false"] explanation {
  border: 2px solid lightpink;
  border-top: 0;
}

questionnaire .wrapper-answer:hover {
  cursor: pointer;
  background-color: #eee;
}

questionnaire answer p {
  margin: 0 0 0 16px;
  padding: 6px;
  /*font-size: 12pt;
  border: 1px solid #000;
  width:100%;*/
}


questionnaire explanation {
  display: none;
  /*max-width: 30vw;*/
}

questionnaire answer [visible=true] {
  margin: 5px 0;
  padding: 15px 12px;
  font-size: 12pt;
  word-break: break-word;
  border:0;
  background-color: #fdfdfd;
}

questionnaire answer [visible=true] p {
  border: 0;
}

questionnaire .answer-mark, questionnaire .expander, questionnaire .collapser {
  width: 1.5em;
  align-self: center;
}

questionnaire .answer-mark-selected {
  display: none;
}

questionnaire answer[selected="true"] .answer-mark-selected {
  display: inline-flex;
  margin-left: -1.5em;
}

questionnaire p {
  margin: 0;
  align-self: center;
}

questionnaire .wrapper-answer > div, questionnaire .question-header > div {
  margin: 0.5em;
  align-self: center;
}

/* RESPONSIVE MAX WIDTH */
@media (min-width: 768px) {
  questionnaire question, questionnaire .summary {
    max-width: 600px;
  }
}

/* FEEDBACK */
/* text */
questionnaire .correct-text, questionnaire .wrong-text {
  display: inline-flex;
  width: 100%;
  justify-content: center;
  display: none;
}
questionnaire question[answer="correct"] .correct-text {
  display: inline-flex;
  color: darkgreen;
}
questionnaire question[answer="wrong"] .wrong-text {
  display: inline-flex;
  color: darkred;
}

/* question border */
questionnaire question[answer="pending"] {
  border: 1px solid silver;
}

questionnaire question[answer="correct"] {
  border: 1px solid green;
}

questionnaire question[answer="wrong"] {
  border: 1px solid darkred;
}

/* answers */
questionnaire question[answer="pending"] answer[selected="true"] .wrapper-answer {
  border: 2px solid grey;
}
/* *="o" means correct or wrong, not pending */
questionnaire question[answer*="o"] answer[correct="true"][selected="true"] .wrapper-answer {
  border: 2px solid green;
}
questionnaire question[answer*="o"] answer[correct="true"] .wrapper-answer {
  background-color: lightgreen;
}
questionnaire question[answer*="o"] answer[correct="false"][selected="true"] .wrapper-answer {
  border: 2px solid darkred;
  background-color: lightpink;
}

/* explanation */
/* manually expanded explanation */
questionnaire .collapser, questionnaire .expander {
  display: none;
}
questionnaire .answer-text-container {
  flex-grow: 4;
}

questionnaire question[answer*="o"] .expander {
  display: block;
}

questionnaire question[answer*="o"] answer[expanded="true"] explanation {
  display: block;
}
questionnaire question[answer*="o"] answer[expanded="true"] .expander {
  display: none;
}
questionnaire question[answer*="o"] answer[expanded="true"] .collapser {
  display: block;
}

/* NAVIGATION */
/* only show question that has been set visible */
questionnaire question{
  display:none;
}
questionnaire .summary {
  display: none;
}
questionnaire [visible=true]{
  display:block;
}

/* button styles */
questionnaire .submit-button, questionnaire .next-button, questionnaire .reset-button {
  padding:15px;
  margin:5px 15px;
  margin-top: 15px;
  border: 4px solid #bbb;
  border-radius: 7px;
  font-size:1.3em;
}
questionnaire .submit-button:hover, questionnaire .next-button:hover, questionnaire .reset-button:hover {
  background-color: #bbb;
  cursor:pointer;
}

/* show submit-button for unanswered multiplechoice questions */
questionnaire .submit-button {
  display: none;
}
questionnaire question[type="multiplechoice"][answer="pending"] .submit-button {
  display: block;
}

/* show next-button for answered questions */
questionnaire .next-button {
  display: block;
}
questionnaire question[answer="pending"] .next-button {
  display: none;
}
questionnaire question:last-child .next-button {
  display: none;
}

/* NAVIGATION/SUMMARY BUBBLES */
questionnaire .question-overview {
  display: flex;
  justify-content: center;
  align-items: center;
  margin-bottom: -1em;
  z-index: 1000;
}
questionnaire .bubble {
  height: 1em;
  width: 1em;
  border: 1px solid transparent;
  border-radius: 0.5em;
  display: inline-flex;
  margin: 0 0.25em;
}
questionnaire .bubble:hover {
  cursor: pointer;
  transform: scale(0.9);
}
questionnaire .bubble-pending {
  background-color: white;
  border-color: silver;
}
questionnaire .bubble-current {
  height: 2em;
  width: 2em;
  border-radius: 1em;
  background-color: azure;
}
questionnaire .bubble-correct {
  background-color: lightgreen;
  border-color: darkgreen;
}
questionnaire .bubble-wrong {
  background-color: lightpink;
  border-color: darkred;
}

/* SUMMARY */
questionnaire .summary[visible="true"] {
  display: block;
  text-align: center;
  border: 1px solid silver;
}
questionnaire .summary-bar-container {
  width: 100%;
  background-color: azure;
  margin: 1em;
  margin-left: 0;
  margin-right: 0;
}
questionnaire .summary-bar {
  background-color: lightgreen;
  border: 1px solid darkgreen;
  padding: 0.25em;
  width: 1%;
  transition: all 10s ease-out;
}
questionnaire[current_question="0"] .question-overview{
  margin-bottom: -0.5em;
}`;
})(Ressources || (Ressources = {}));
