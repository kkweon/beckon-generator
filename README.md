# beckon-generator

[![Build Status](https://travis-ci.com/kkweon/beckon-generator.svg?branch=master)](https://travis-ci.com/kkweon/beckon-generator)

## Help

```shell
beg --help
```

```
Beckon AngularJS Component Generator

Usage: beg MODULE NAME [-S|--spec] [--service]
  Generate AngularJS Beckon Component

Available options:
  MODULE NAME              Beckon Module Name (e.g., beckon.steel.answerPage)
  -S,--spec                Generate a spec file
  --service                Generate a service file
  -h,--help                Show this help text
```


## QuickStart

```shell
beg steel.answerPage --spec
```

will generate

```
./src/main/resources/com/beckon/steel/answerPage/answerPage.js
./src/main/resources/com/beckon/steel/answerPage/answerPage.tmpl
./src/test/javascript/unit/steel/answerPage/answerPageSpec.js
```
