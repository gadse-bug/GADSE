# GADSE

## Benchmarks

We place our benchmarks and implementation in a docker image. The information about our benchmarks is shown below.

| Subject      | <span id="Location">Location</span>                 | <a id="Entry Point">Entry Point</a>                 | <a id="Tokenizer Function">Tokenizer Function</a>            | <a id="Token Definition">Token Definition</a>      |
| ------------ | --------------------------------------------------- | --------------------------------------------------- | ------------------------------------------------------------ | -------------------------------------------------- |
| Clojure      | /root/jpf/jpf-jdart/src/example-javacc-clojure      | test.TestClojure#start                              | clojure.ClojureParserTokenManager#getNextToken               | clojure.ClojureParserConstants                     |
| FirstOrder   | /root/jpf/jpf-jdart/src/example-firstorderparser    | de.dominicscheurer.fol.test.TestFirstOrder#start    | de.dominicscheurer.fol.parser.FOLParserTokenManager#getNextToken | de.dominicscheurer.fol.parser.FOLParserConstants   |
| JsonParser   | /root/jpf/jpf-jdart/src/example-jsonparser-javacc   | test.TestJsonparser#start                           | jsonparser.JSONParserTokenManager#getNextToken               | jsonparser.JSONParserConstants                     |
| J2Latex      | /root/jpf/jpf-jdart/src/example-j2latex             | test.TestJ2Latex#start                              | com.github.situx.compiler.parser.C1TokenManager#getNextToken | com.github.situx.compiler.parser.C1Constants       |
| SiXpath      | /root/jpf/jpf-jdart/src/example-sixpath             | test.sixpath.TestSixpath#start                      | de.fzi.XPath.Parser.XPathParserTokenManager#getNextToken     | de.fzi.XPath.Parser.XPathParserConstants           |
| Aejcc        | /root/jpf/jpf-jdart/src/example-aejcc               | test.TestAejcc#start                                | ca.ubc.cs411.aejcc.parser.AEParserTokenManager#getNextToken  | ca.ubc.cs411.aejcc.parser.AEParserConstants        |
| Jsicc        | /root/jpf/jpf-jdart/src/example-jsijcc              | test.TestJsijcc#start                               | javascriptInterpreter.parser.JavascriptTokenManager#getNextToken | javascriptInterpreter.parser.JavascriptConstants   |
| FastJSON     | /root/jpf/jpf-jdart/src/example-fastjson-dev        | test.testFastjsonDev#start                          | com.alibaba.fastjson.parser.JSONLexer#token                  | com.alibaba.fastjson.parser.JSONToken              |
| Bling        | /root/jpf/jpf-jdart/src/example-bling               | test.TestBling#start                                | com.cloudability.bling.ast.BlingParserTokenManager#getNextToken | com.cloudability.bling.ast.BlingParserConstants    |
| Calculator   | /root/jpf/jpf-jdart/src/example-javacc-calculator   | test.TestCalculator#start                           | com.braxisltd.calculator.ArithmeticParserTokenManager#getNextToken | com.braxisltd.calculator.ArithmeticParserConstants |
| HtmlParser   | /root/jpf/jpf-jdart/src/example-html.parser         | test.TestHtmlParser#start                           | html.parser.testTokenManager#getNextToken                    | html.parser.testConstants                          |
| UriParser    | /root/jpf/jpf-jdart/src/example-urijavacc           | test.TestUriParser#start                            | uri.ParserTokenManager#getNextToken                          | uri.ParserConstants                                |
| Jsonmwn      | /root/jpf/jpf-jdart/src/example-jsonparser-mwnorman | org.mwnorman.json.test.TestJsonParserMwnorman#start | org.mwnorman.json.JSONParserTokenManager#getNextToken        | org.mwnorman.json.JSONParserConstants              |
| OaJava       | /root/jpf/jpf-jdart/src/example-oajavaparser        | test.TestOajavaParser#start                         | com.viaoa.javaparser.JavaParserTokenManager#getNextToken     | com.viaoa.javaparser.JavaParserConstants           |
| JavaParser   | /root/jpf/jpf-jdart/src/example-javaparser          | test.TestJavaparser#start                           | japa.parser.ASTParserTokenManager#getNextToken               | japa.parser.ASTParserConstants                     |
| CMMParser    | /root/jpf/jpf-jdart/src/example-javaccgrammar       | test.TestCmmparser#start                            | rong.CMMParserTokenManager#getNextToken                      | rong.CMMParserConstants                            |
| Curta        | /root/jpf/jpf-jdart/src/example-curta               | test.TestCurta#start                                | nl.bigo.curta.CurtaParserTokenManager#getNextToken           | nl.bigo.curta.CurtaParserConstants                 |
| SqlParser    | /root/jpf/jpf-jdart/src/example-sqlparser           | sql.TestSqlParser#start                             | sql.ParserTokenManager#getNextToken                          | sql.ParserConstants                                |
| JsonRaupachz | /root/jpf/jpf-jdart/src/example-json-raupachz       | test.TestJsonRaupachz#start                         | parser.JSONTokenManager#getNextToken                         | parser.JSONConstants                               |

## Running Benchmark

```bash
docker pull gadse/jdart:v1
docker run --name Jdart -it gadse/jdart:v1 /bin/bash

cd /root/jpf/jpf-jdart
./run_benchmark.sh
```

This will produce `/root/jpf/jpf-jdart/coverage.csv` which reports coverage information of our benchmark. The contents in `coverage.csv` is like:

```
Program,Stategy,Method,Branch,Statement
TestAejccDriver,dfs,baseline,118,320
TestAejccDriver,dfs,gadse,125,335
```

## Stage Separation

We separate parsing program into `stage1_2()` and `stage3()`, which are called by entry function `start()`. The details are as follows: 

```java
public void start() throws ParseException {
    stage1_2();
    stage3();
}
```

Please note some benchmarks only parse the inputs and have no other function code, Such benchmarks cannot be separated by this way. The benchmarks that have been separated into different stages are:

- Calculator
- Curta
- FastJSON
- FirstOrder
- J2Latex
- JavaParser
- Jsicc
- OaJava

## Grammar

We implement the grammar of our benchmarks, which are  used for running [Grammar Fuzzing](https://github.com/havrikov/tribble).

| Grammar File                | Benchmark                                   |
| --------------------------- | ------------------------------------------- |
| arithmetic-aejcc.scala      | Aejcc                                       |
| arithmetic-bling.scala      | Bling                                       |
| arithmetic-calculator.scala | Calculator                                  |
| arithmetic-curta.scala      | Curta                                       |
| Clojure.scala               | Clojure                                     |
| java.scala                  | J2Latex, OaJava, JavaParser                 |
| sql.scala                   | SqlParser                                   |
| cmm.scala                   | CMMParser                                   |
| Json.scala                  | JsonParser, FastJSON, Jsonmwn, JsonRaupachz |
| uri.scala                   | UriParser                                   |
| FirstOrder.scala            | FirstOrder                                  |
| js.scala                    | Jsicc                                       |
| xpath.scala                 | SiXpath                                     |
| html.scala                  | HtmlParser                                  |




## Question & Answer

### 1 Where the benchmarks can be obtained

> We place our benchmarks with input and token symbolization into `/root/jpf/jpf-jdart/src/`. The details of each benchmark are shown in Benchmarks table [Location](#Location) column.

### 2 Which are the entry points of the symbolic execution

> It is shown in Benchmarks table [Entry Point](#Entry Point) column.

### 3 Which is the tokenizer functions

> It is shown in Benchmarks table [Tokenizer Function](#Tokenizer Function) column.

### 4 What was the manual help needed for the separation of the parsing program into different stages 

> The details are shown in Benchmarks table [Stage Separation](#Stage Separation) column.

### 5 Which are the 11 grammars

> The details are shown in [Grammar](#Grammar).

### 6 Which tokens there are

> The details are shown in Benchmarks table  [Token Definition](#Token Definition) column.