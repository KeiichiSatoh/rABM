---
title: "10. Series"
format: html
number-sections: False
editor: visual
---

::: callout-note
ここまで解説した機能でABMを回すことはできますが、この章では一連の設定を自動化して実行するSeriesという機能を紹介します。はじめにSeriesについて概観し、実際に簡単な例で実際の使い方を解説します。
:::

## 10.1 Seriesについて

### (1) 目的

**Series**はここまで解説した一連のrABMの設定や実行に関するスクリプトを**chunk**として保存し、任意の**chunk**を組み合わせて実行する機能です。この機能は大きく二つの目的で使用することができます。

- **Game**オブジェクトを`run_Game`によって回すだけではなく、変数やフィールドの設定からの一連の流れで毎回のシミュレーションを回す場合

- ABMの実行が一度の`run_Game`を実行するだけでは終了せず、複数の`run_Game`を組み合わせる必要がある場合（たとえばいったん収束値に至ったうえで、そこに何らかのランダムな変化を\*\*State\*\*フィールドに与えて、さらに`run_Game`を実行する場合など）

### (2) 基礎的な使用方法

SeriesはABMに関するものだけではなく、通常のスクリプトに関しても使用することができます。概要を掴むうえでは、こちらの方が分かりやすいため、以下の例では、通常の簡単なスクリプトを**Series**として実行しています。


::: {.cell}

```{.r .cell-code}
library(rABM)
```
:::


はじめに各スクリプトを`Chunk({...})`としてくくります。なお1行のみのスクリプトは`Chunk(x <- 1)`のように書いても内部で処理されますが2行以上の場合にはエラーになります。したがって、常に`{ }`でくくる癖をつけたほうが無難です。


::: {.cell}

```{.r .cell-code}
# チャンクを
step1 <- Chunk({
  x <- 1
})

step2 <- Chunk({
  y <- x + 1
})

step3 <- Chunk({
  z <- y + 1
})
```
:::


これらの各Chunkを`Series()`として定義することで、**Series**オブジェクトが作成されます。


::: {.cell}

```{.r .cell-code}
S <- Series(step1, step2, step3)
```
:::


このSeriesオブジェクトの中身は、通常通りプリントすることで確認できます。


::: {.cell}

```{.r .cell-code}
S
```

::: {.cell-output .cell-output-stdout}

```
<Series>
[chunks]
$step1
{
    x <- 1
}

$step2
{
    y <- x + 1
}

$step3
{
    z <- y + 1
}

[default]
  (none)
-------------------
n of chunks  : 3 
  chunks     : step1, step2, step3 
n of defaults: 0 
  defaults   :  
-------------------
```


:::
:::


**Series**オブジェクトは、`run_Series()`関数で、実行することができます。


::: {.cell}

```{.r .cell-code}
S2 <- run_Series(S)
```

::: {.cell-output .cell-output-stdout}

```
[Series plan]
step1 -> step2 -> step3 

Series implementing...
  Step 1: step1
  Step 2: step2
  Step 3: step3
Series implementation DONE!
```


:::
:::


アウトプットが格納された`S2`オブジェクトには、`run_Series`が実行された環境下での値が`$values`という中に格納されています。


::: {.cell}

```{.r .cell-code}
S2
```

::: {.cell-output .cell-output-stdout}

```
$values
$values$z
[1] 3

$values$y
[1] 2

$values$x
[1] 1


$series_plan
[1] "step1" "step2" "step3"

$series_seed
[1] 1785891777

$error_log
list()

$implementation_took
[1] "00:00:00.000"
```


:::
:::


## 10.2 応用的な利用方法

### (1) 複数のChunkを用意しておき、runごとに変える

`run_Series`では実際に利用するChunkを`series_plan`で指定することができます。このことを利用して、あらかじめ複数のchunkを用意することで、分析ごとに簡単に変えることができます。


::: {.cell}

```{.r .cell-code}
# step1_1とstep1_2を両方用意しておく
step1_1 <- Chunk({x <- 1})
step1_2 <- Chunk({x <- 10})
step2   <- Chunk({y <- x^2})

# Series
S <- Series(step1_1, step1_2, step2)

# step1_1の方を利用して計算する
S2 <- run_Series(S, series_plan = c("step1_1", "step2"))
```

::: {.cell-output .cell-output-stdout}

```
[Series plan]
step1_1 -> step2 

Series implementing...
  Step 1: step1_1
  Step 2: step2
Series implementation DONE!
```


:::
:::


上記の例と同様に、`step1_2`の方を指定することで結果を変えられます。

### (2) default値を指定しておく

あらかじめdefault値を指定しておくことで、`run_Series`の実行時に、その値が環境に存在することを前提に計算がなされます。


::: {.cell}

```{.r .cell-code}
# x = 2というdefault値を入れておく
step2 <- Chunk({y <- x^2})
S <- Series(step2, default = list(x = 2))

# step2だけ回しても、x = 2が環境に存在するため、計算が可能となる
S2 <- run_Series(S, series_plan = "step2")
```

::: {.cell-output .cell-output-stdout}

```
[Series plan]
step2 

Series implementing...
  Step 1: step2
Series implementation DONE!
```


:::

```{.r .cell-code}
S2
```

::: {.cell-output .cell-output-stdout}

```
$values
$values$y
[1] 4

$values$x
[1] 2


$series_plan
[1] "step2"

$series_seed
[1] 1785891777

$error_log
list()

$implementation_took
[1] "00:00:00.000"
```


:::
:::


Default値を`run_Series`の引数`input`を変えることで変更できる。


::: {.cell}

```{.r .cell-code}
S3 <- run_Series(S, series_plan = "step2", input = list(x = 3))
```

::: {.cell-output .cell-output-stdout}

```
[Series plan]
step2 

Series implementing...
  Step 1: step2
Series implementation DONE!
```


:::

```{.r .cell-code}
S3
```

::: {.cell-output .cell-output-stdout}

```
$values
$values$y
[1] 9

$values$x
[1] 3


$series_plan
[1] "step2"

$series_seed
[1] 1785891777

$error_log
list()

$implementation_took
[1] "00:00:00.000"
```


:::
:::


### (3) 結果変数のみ取得する

`run_Series`関数の`simplify_output`引数を`TRUE`にすると、シミュレーションに変数のみアウトプットで返します。


::: {.cell}

```{.r .cell-code}
S4 <- run_Series(S, series_plan = "step2", simplify_output = TRUE, verbose = FALSE)
S4
```

::: {.cell-output .cell-output-stdout}

```
$y
[1] 4

$x
[1] 2
```


:::
:::


## 10.3 実際のABMでの使用用法

本章の最後に実際のABMでの使用方法について、ごく簡単な例を用いて解説します。

以下のモデルにおいて更新そのものはエージェントに毎回1が足されることが共通ですが、エージェントの初期値を変えたいと思います。このようなときにSeriesが使用できます。


::: {.cell}

```{.r .cell-code}
# 二種類の初期値を用意する
money_pattern1 <- Chunk({money <- 1:5})
money_pattern2 <- Chunk({money <- 100:105})

# 後の操作は共通
create_G <- Chunk({
  # act_FUNを作成する
  add_money <- function(){self$money <- self$money + 1}
  
  # Gameオブジェクトに格納する
  G <- Game(State(money), Act(add_money))
})

run <- Chunk({G2 <- run_Game(G, plan = c("add_money"))})

# Seriesとして定義
S <- Series(money_pattern1, money_pattern2, create_G, run)

# money_pattern2の方で回してみる
S2 <- run_Series(S, series_plan = c("money_pattern2", 
                                    "create_G",
                                    "run"),
                 verbose = FALSE,
                 simplify_output = TRUE)
```

::: {.cell-output .cell-output-stdout}

```
[plan] 
add_money
 
[stop_FUN] 
stop times at 2

The initial values at time 1 were saved.

Ready to run......
   start time  : 1
   current time: 2
Finished at time 2 

Simulation took 00:00:00.016 (hh:mm:ss.mmm)
```


:::

```{.r .cell-code}
# 結果を確認してみる
S2$G2$money
```

::: {.cell-output .cell-output-stdout}

```
[1] 101 102 103 104 105 106
```


:::
:::


