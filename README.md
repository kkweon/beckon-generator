# beckon-generator

[![Build Status](https://travis-ci.com/kkweon/beckon-generator.svg?branch=master)](https://travis-ci.com/kkweon/beckon-generator)

## Help

```shell
> beg --help

Usage: beg [-V|--version] COMMAND
  Beckon Generator CLI

Available options:
  -h,--help                Show this help text
  -V,--version             show version

Available commands:
  react                    Generate React file
  ng                       Generate AngularJS file
```

### beg react

```shell
> beg react --help

Usage: beg react module/component [--no-src] [--spec] [--story] [-f|--force]
  Generate React file

Available options:
  module/component         Module name can be atoms/molecules/organisms. (e.g.,
                           atoms/BrPageHeader)
  --no-src                 Do not generate a src file
  --spec                   Generate a spec file
  --story                  Genereate a Storybook story file
  -f,--force               Overwrite when a target file exists
  -h,--help                Show this help text
```

### beg ng

```shell
> beg ng --help

Usage: beg ng MODULE NAME ([-S|--spec] | [--spec-only]) [--service] [--force]
              [--old-typescript]
  Generate AngularJS file

Available options:
  MODULE NAME              Beckon Module Name (e.g., beckon.steel.answerPage)
  -S,--spec                Generate a spec file along with JS/TS file
  --spec-only              Generate a spec file only
  --service                Generate a service file
  --force                  Force (overwrite if file exists)
  --old-typescript         Generates an old (namespace) TypeScript
  -h,--help                Show this help text
```


## QuickStart

```shell
> beg ng steel.answerPage --spec
```

will generate

```
./src/main/resources/com/beckon/steel/answerPage/answerPage.js
./src/main/resources/com/beckon/steel/answerPage/answerPage.tmpl
./src/test/javascript/unit/steel/answerPage/answerPageSpec.js
```
