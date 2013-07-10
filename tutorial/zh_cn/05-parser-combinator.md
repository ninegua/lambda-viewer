#手把手教你做λ（五）语法识别器的拼接与组合

上一节里我们实现了针对单个字母的变元识别器 (parser)，它是这样的：

      type ReadS a = String -> [(a, String)]

      char :: (Char -> Bool) -> ReadS Char
      char f (c:s) | f c = [(c, s)]
      char f _           = []

      variable :: ReadS Var
      variable s = map f (char isAlpha s)
        where f (c, s1) = (V [c], s1)

类型 `ReadS a` 所代表的就是对类型 `a` 的识别器，`char` 用于识别单个字符，`variable` 则通过 `map f` 将 `char` 所识别的单个字符转为我们所需要的 `Var` 类型。这种从 `ReadS a` 到 `ReadS b` 的 map 转换方式会经常用到，让我们把它提出来单独定义为：

      mapP :: (a -> b) -> ReadS a -> ReadS b
      mapP f = map (\ (c, s) -> (f c, s))

那么 `variable` 可以写成：
      
      variable = mapP (\c -> V [c]) (char isAlpha)

注意这里连参数 `s` 被省略掉了，那是因为 `char isAlpha` 的类型就是 `ReadS Char`，而匿名函数 `\c -> V [c]` 的类型是 `Char -> Var`，所以通过 `mapP` 我们正好得到了 `variable` 函数所需的类型 `ReadS Var`。

用 `char` 做出来的识别器仅能识别单个字符，在本节我们要考虑如何将它扩展成可以识别多个字符 (比如开头用字母，后面用数字) 的变元识别器。

一种常见的解决方法是考虑如何根据给定的条件切割字串， 这用 Prelude 里面的 `takeWhile`, `dropWhile` 或者 `span` 函数就可以做到。但是在这里，我们不妨换个角度，先来考虑一下是否有更普适 (general) 的方案。

既然我们已经可以做出单个字符的识别器，是否可以仅用它来搭建更复杂的识别器呢？而识别器是否能够互相拼接起来，又有哪几种拼接方式？最基础的识别器都有哪些？

先来回答最后一个问题。既然我们考虑的识别器都是以字串 (字符的列表) 为输入类型，那么显而易见，必要的一个基础识别器是识别单个字符，这个就是我们已经实现了的 `char` 函数。然后相应地，识别空字串也应该是一个基础识别器：

      nil :: ReadS [a]
      nil s = [([], s)]

对于任何输入，`nil` 都可以成功识别，并且返回识别值 (就是空表)，以及剩余的字串 (就是原字串)。由于空表可以代表任意类型的列表，所以 `nil` 代表的是对任意列表类型 `[a]` 的空表识别器。

现在我们有两个基础识别器了，再来看看它们的拼接组合方式有哪些。根据上下文无关文法 (Context Free Grammar，缩写为 CFG) 的结构，应该至少有两种方式：序列和分支。

序列是假设有两个识别器 `f :: ReadS a` 和 `g :: ReadS b`，先用 `f` 识别，然后用 `g` 识别剩下的字串，这样我们得到一个 `a` 类型和一个 `b` 类型的值，放在一起就是类型为 `(a, b)` 的双元组 (也叫 tuple)。借助上一章学习的列表推导式，序列方式的组合可以写成下面的函数：

      (&&&) :: ReadS a -> ReadS b -> ReadS (a, b)
      f &&& g = \s -> [ ((x, y), s2) 
                      | (x, s1) <- f s, 
                        (y, s2) <- g s1 ]

这里我们定义了一个中缀 (infix) 操作符函数 `&&&` 来拼接 `f` 和 `g`。因为 Haskell 对操作符默认是中缀的方式，所以在等式左侧写成 `f &&& g` 实际上表示的是对函数 `&&&` 的定义，而不是对 `f` 的定义。这里 `f` 和 `g` 则是此函数的参数。中缀操作符如果用括号括起来，则可以像其它前缀函数一样自由使用，例如，用于声明它的类型。

在等式右边则使用了列表推导，先用 `f` 在 `s` 中识别出 `x :: a`，再用 `g` 在剩余字串 `s1` 中识别出 `y :: b`，然后将 `(x, y)` 组合成列表返回得到 `ReadS (a, b)` 类型。注意等式右边首先是一个以字串为参数的匿名函数，因为 `f &&& g` 的类型是 `ReadS (a, b)`，展开即为函数类型 `String -> [(a,b), String]`。

接下来考虑分支组合。我们可以将 `f :: ReadS a` 和 `g :: ReadS b` 组合成 `ReadS (Either a b)`。这个思路非常自然，因为前面章节里，我们提到过数据类型的两个基本结构是 product 和 sum，类型 `(a, b)` 表达的是前者，而后者则可以用类型 `Either a b` 表达，它在 Prelude 里面定义如下：

      data Either a b = Left a | Right b

亦即“左”分支表示 `a` 类型，“右”分支表示 `b` 类型，两者选其一。

语法识别器无可避免地要在程序中构建数据类型，甚至可以看作是从字串起始来构建数据类型，所以它们的组合方式类似于 product 和 sum 也就不奇怪了。但在分支组合时，如果 `f` 和 `g` 都能够成功识别那该怎么办？而 `Either` 似乎不能够同时代表个值，那么简单的办法是优先选择其中一个。假设我们选择倾向于 `f`，写成函数如下：

      (|||) :: ReadS a -> ReadS b -> ReadS (Either a b)
      f ||| g = \s -> case f s of
                        [] -> map right (g s)
                        xs -> map left xs
        where left  (x, s) = (Left  x, s)
              right (y, s) = (Right y, s)

这里 `|||` 也是我们定义的中缀操作符函数，当 `f s` 成功识别时 (返还值不为空表)，我们用 `map left` 将 `xs :: [(a, String)]` 作为左分支转为 `[(Either a b, String)]` 类型；否则我们则用 `map right` 将 `g s` 的识别值 (当 `g` 识别失败时，返回的空表不影响 `map` 操作) 作为右分支也转为同样的 `Either` 类型。

考虑到在进行分支识别时，我们识别出来的往往是同类型的值，也就是说 `a` 和 `b` 是相同类型的时候，这时只需要返回一种类型即可：

      (<|>) :: ReadS a -> ReadS a -> ReadS a
      f <|> g = mapR select (f ||| g)
        where select (Left  x) = x
              select (Right y) = y

这里，`mapR select` 将 `f ||| g` 返还的 `ReadS (Either a a)` 转换为 `ReadS a` 类型。函数 `select` 也可以直接用 Prelude 中 `either` 函数来定义 `select = either id id`。

有了序列和分支这两种基本组合方式，我们可以定义更多样的组合。比如：

      many, many1 :: ReadS a -> ReadS [a]
      many r  = many1 r <|> nil
      many1 r = mapP cons (r &&& many r)
        where cons (x, xs) = x : xs

函数 `many1` 可以用参数所给的识别器 `r :: ReadS a`，来连续从输入字串中识别出一个或多个 `a` 类型的值。`many` 则可以识别零个或多个。值得注意的是，两个定义是相互递归 (mutural recursion)，其中 `many1` 是用 `r &&& many r` 实现，代表用 `r` 和 `many r` 顺序识别，这样就是一个以上。而 `many` 则是 `many1 r <|> nil`，代表一个以上或者没有。因为我们定义了 `<|>` 的操作是左边优先，所以 `nil` 不能放在左边，不然因为它总是成功，会造成返回值恒为空。

有了这样的武器，识别多个字符 (第一个字符为字母，后面可以跟数字) 的变元就简单了：

      variable = mapP f (alpha &&& digits)
        where f (c, d) = V (c : d)
              alpha = char isAlpha
              digits = many1 (char isDigit)

回顾本节开头，正是因为我们没有用 Prelude 中现成的表处理函数，而是去探寻更普适的解决方案，才得以总结出了两种基础识别器，和识别器的两种基本组合方式，加上递归的运用，就能够构建在 CFG 范围内任意复杂的识别器。这种对事物本身规律的探寻和概括，提升抽象 (abstraction) 层面的做法，是编程最的核心能力，在 Haskell 里面的运用尤其突出。

# 习题

一、用组合的方式设计一个识别器 `num :: ReadS Int`，从字串中识别十进制的自然数，正则表达式为 `[0-9]+`。换句话说，是要实现一个和 Prelude 里定义的 `reads :: ReadS Int` 等价的函数。不可以用 `read`，但是可以用 `fromEnum` 来把 `Char` 转为 `Int`。

二、用组合的方式设计一个识别器 `readR :: ReadS Rational`，识别正则表达式 `[+-][0-9]+(%[0-9]+)?` 表示的有理数。换句话说，是要实现一个和 Prelude 里定义的 `reads :: ReadS Rational` 等价的函数。注意 Haskell 里有理数的分子分母之间是用 `%` 符号隔开。

三、上一节讲到 `ReadS a` 的类型可以表达多个识别值，但以上定义的 `<|>` 和 `|||` 的组合方式只能返回其中一个分支的可能性。应该如何定义 `<|>` 或者 `|||` 让它们能够返回两个分支的所有可能性呢？

