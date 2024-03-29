---
name: Bug report
about: peptr produced an error, failure, or otherwise unexpected result.
title: ''
labels: 'bug'
assignees: jeanmanguy

---

## Prework

- [ ] Read and abide by `peptr`'s [code of conduct](https://github.com/jeanmanguy/peptr/blob/master/CODE_OF_CONDUCT.md).
- [ ] Search for duplicates among the [existing issues](https://github.com/jeanmanguy/peptr/issues), both open and closed.
- [ ] Advanced users: verify that the bug still persists in the current development version (i.e. `remotes::install_github("jeanmanguy/peptr")`) and mention the [SHA-1 hash](https://git-scm.com/book/en/v1/Getting-Started-Git-Basics#Git-Has-Integrity) of the [Git commit you install](https://github.com/jeanmanguy/peptr/commits/master).

## Description

Describe the bug clearly and concisely. 

## Reproducible example

Provide a minimal reproducible example with code and output that demonstrates the bug. The [`reprex`](https://github.com/tidyverse/reprex) package is extremely helpful for this.

## Session info

End the reproducible example with a call to `sessionInfo()` in the same session (e.g. `reprex(si = TRUE)`) and include the output.

## Expected output

What output would the correct behavior have produced?
