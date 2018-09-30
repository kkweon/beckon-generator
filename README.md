# beckon-generator

## Help

```shell
beg --help
```

```
Beckon AngularJS Component Generator

Usage: beg MODULE NAME [-S|--spec]
  Generate AngularJS Beckon Component

Available options:
  MODULE NAME              Beckon Module Name (e.g., beckon.steel.answerPage)
  -S,--spec                Generate a spec file
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
