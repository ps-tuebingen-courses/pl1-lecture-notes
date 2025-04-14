# Programming Languages I Lecture Notes
[![Publish GH Page](https://github.com/ps-tuebingen-courses/pl1-lecture-notes/actions/workflows/publish.yml/badge.svg)](https://github.com/ps-tuebingen-courses/pl1-lecture-nodes/actions/workflows/publish.yml)

The published lecture notes are available [Here](http://ps-tuebingen-courses.github.io/pl1-lecture-notes/)


### Required Tools

In order to build locally, you have to install the following tools:

- mdbook [Install Instructions](https://rust-lang.github.io/mdBook/guide/installation.html)

### Building and serving the lecture notes locally

Run the following steps in your console:

```console
sh download_questionnaire_module.sh

./generateMarkdown.sh

./create_rkt_files.sh

./copy_rkt_files.sh

./create_scala_files.sh

./copy_scala_files.sh

mdbook build

mdbook serve --open
```

### How to add Questionnaire / interactive questions to this script
For more information look at the [documentation of the Questionnaire Module](https://github.com/se-tuebingen/interactive-textbooks#how-to-use).
