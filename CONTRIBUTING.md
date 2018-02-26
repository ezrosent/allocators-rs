<!-- Copyright 2017-2018 the authors. See the 'Copyright and license' section of the
README.md file at the top-level directory of this repository.

Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
the MIT license (the LICENSE-MIT file) at your option. This file may not be
copied, modified, or distributed except according to those terms. -->

Contributing
============

Hi there! Interested in contributing? We'd love to have you!

## Getting started

If you're new to Rust or to this project and just want an easy issue to get started with, check out the issues with the [experience-easy](https://github.com/ezrosent/allocators-rs/issues?q=is%3Aissue+is%3Aopen+label%3Aexperience-easy) label. If you have any questions about anything, feel free to contact [joshlf](https://github.com/joshlf) or [ezrosent](https://github.com/ezrosent).

### Finding something to fix/improve

All issues are labeled with the difficulty ([experience-easy](https://github.com/ezrosent/allocators-rs/issues?q=is%3Aissue+is%3Aopen+label%3Aexperience-easy), [experience-medium](https://github.com/ezrosent/allocators-rs/issues?utf8=%E2%9C%93&q=is%3Aissue%20is%3Aopen%20label%3Aexperience-medium%20), and [experience-hard](https://github.com/ezrosent/allocators-rs/issues?utf8=%E2%9C%93&q=is%3Aissue%20is%3Aopen%20label%3Aexperience-hard%20)) and the component of the project that they apply to (e.g., [crate-elfmalloc](https://github.com/ezrosent/allocators-rs/issues?q=is%3Aissue+is%3Aopen+label%3Acrate-elfmalloc), [travis-ci](https://github.com/ezrosent/allocators-rs/issues?utf8=%E2%9C%93&q=is%3Aissue%20is%3Aopen%20label%3Atravis-ci), etc). Use label search or browse the issues to find something you're interested in.

Once you've found an issue to work on, please comment on it so that people know you're working on it and don't duplicate your work.

### Mentoring

Since we're a small project, we haven't assigned particular mentors for particular issues. However, we're happy to mentor anybody working on any issue, so if you'd like mentorship on working through an issue, don't hesitate to reach out!

## Submitting a change

We use GitHub PRs to accept and review changes. In order to submit a change:
- Fork this repository
- Make the change in your fork
- Open a PR with your changes

PRs should abide by the following guidelines, but if you aren't sure about how to do that, feel free to submit a work-in-progress PR and we'll help you out.

### Tests
The following tests are automatically run by Travis CI when a PR is submitted. Since Travis can be slow, it's probably best to make sure they pass by running them locally before pushing changes.
- `cargo clippy` must not show any warnings
- `cargo test` must pass
- If there are any features for the crate, then `cargo clippy` and `cargo test` should also be run with each of the features enabled

Please add tests for any significant changed or added features. If you're not sure where tests should go or how they should be written, just ask and we'd be happy to help!

### API Design
Providing clean, ergonomic APIs are important. If you're adding or modifying APIs, please take care to design them with the user in mind. See the [Rust API Guidelines](https://rust-lang-nursery.github.io/api-guidelines/) for more detailed recommendations.

### Documentation
All public-facing APIs should have documentation which abides by [RFC 505](https://github.com/rust-lang/rfcs/blob/master/text/0505-api-comment-conventions.md). Try running `cargo doc --open` to compile and view documentation in the browser to see if it looks as you expect.

### Commit messages
Commit messages should be of the form `<component>: <description>`, where the description is written in the third person singular present imperative form, and doesn't have a period at the end. What a mouthful! Basically, this means a description like "Do the thing." For example, `slab-alloc: Add documentation comments`.

### `CHANGELOG.md` file
Each crate has its own `CHANGELOG.md` file, and unreleased changes are tracked in the "Unreleased" section at the top of the file. Any changes that you make should be added to this section so that they can be incorporated into the changelog entry for the next release. Our changelogs follow the [keep a changelog](http://keepachangelog.com/) format.

### Copyright comment
Every new file should bear the following copyright comment at the top, preceding any other text:

```rust
// Copyright 2017 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.
```

Modified files should have the copyright dates updated. For example, if a file's comment reads `Copyright 2017`, but it is modified in 2018, then the comment should be updated to read `Copyright 2017-2018`.

### Rebasing
All changes should be rebased onto the master branch so that we maintain a linear history. If you're not sure how to do that, just ask! For a good introduction to rebasing and why it's helpful, see [here](https://git-scm.com/book/en/v2/Git-Branching-Rebasing).

## Conduct

We follow the [Rust Code of Conduct](https://www.rust-lang.org/en-US/conduct.html).
