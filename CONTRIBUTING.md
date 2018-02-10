# Contributing

We love pull requests from everyone. Instructions:

Fork, then clone the repo:

    git clone git@github.com:your-username/bazel-deps.git

Make sure the tests pass:

    bazel test //...

Make your change. Add tests for your change. Make the tests pass:

    bazel test //...

If your change affects the generated `workspace.bzl`, regenerate it:

    bazel run //:parse -- generate -r $PWD \
        -s 3rdparty/workspace.bzl -d dependencies.yaml


Push to your fork and [submit a pull request][pr].

[pr]: https://github.com/johnynek/bazel-deps/compare/

At this point you're waiting on us. We may suggest some changes or improvements
or alternatives.

Some things that will increase the chance that your pull request is accepted:

* Write tests.
* Write a [good commit message][commit].

[commit]: http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html

These instructions were copied from [factory_bot_rails][factory_bot_rails].

[factory_bot_rails]: https://github.com/thoughtbot/factory_bot_rails
