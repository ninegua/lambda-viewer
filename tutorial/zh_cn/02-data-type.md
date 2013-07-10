#手把手教你做λ（二）数据类型的结构与解构


如果你熟悉[BNF]语法，把λ表达式的结构写出来就是这样的：

      <term>  ::=   <var> | "λ" <var> "." <term> | 
                    <term> " " <term>
       <var>  ::=   "a" | "b" | ... | "y" | "z" | ...

前面说过，我们的目标是实现一个[λ表达式的计算器](http://projectultimatum.org/cgi-bin/lambda)。那么在 Haskell 里面如何表示一个λ项呢？我们可以定义下面的数据类型(data type):

      data Term = Var Var | Lam Var Term | App Term Term 
      data Var  = V String

其中紧跟在关键字 `data` 后的 `Term` 和 `Var` 是我们定义的类型名字，等式右边 `Var`，`Lam`，`App` 是 `Term` 类型的 constructor (构造子)，`V` 是 `Var` 类型的构造子。在 Haskell 里，构造子和类型都要用大写字母开头，而且不禁止构造子和类型名称重复，所以需要根据上下文来判断一下。构造子属于 value 层面，类型属于 type 层面，略为注意就不会混淆。例如第一行的 `Var Var` 里面，前者是构造子，后者是类型；而 `Lam Var Term` 这里，第一个 `Lam` 是构造子，后面的 `Var` 和 `Term` 都是类型。

不难发现，在 Haskell 里定义λ项的数据类型和它的 BNF 语法有些类似。不同之处是前者需要对每种λ项的形态提供一个构造子。这样做的目的是严格区分不同的类型，或者同一个类型的不同部分。这在构造类型的值，或者进行 pattern matching (模式匹配)时都是必要的。

有了以上的定义，我们可以在 Haskell 里面表达任意的λ项了。比如：

- `Var (V "x")` 代表变元形态的λ项 x
- `Lam (V "f") (Lam (V "x") (App (Var (V "f")) (Var (V "x"))))` 代表 λf.(λx.(f x))，或者简写为 λf.λx.f x

这样构建出来的λ项，也叫做 `Term` 类型的值。我们不妨把类型看作是一个集合 (set)，里面包括各种可以用它的构造子们合法构建的值。比如 `Integer` 类型的值可以是任意整数，构造子为 `0|1|2|...`；而 `String` 类型的值可以是任意长度的字符串。

在 Haskell 里用 `data` 关键字定义的数据类型，也叫做代数数据类型 (Algebraic Data Type, 或简称 ADT)，可以很方便地对各种抽象结构进行表述。定义和运用 ADT 也是高级编程语言的一个基本功能。

数据类型就是数据类型好了，为什么叫 algebraic？一方面是因为可以用自定义的类型名字，例如上面的 `Term` 和 `Var`，来指代它们所定义的类型，以构建更多的类型。例如 `Var` 类型就被用来定义 `Term`，而 `Term` 的定义里还用到了它自己，构成一个递归结构。

那么代数数据类型的基本结构是什么？从上面的例子，我们至少可以观察到以下两种：

1. Product (积)。如果 A、B 指代两种不同的类型，那么 A×B 表示两者的积类型，它的值里面，同时要有一个 A 类型的值与一个 B 类型的值。通常所说的 tuple 类型就是这个意思。

2. Sum (和)。如果 A、B 指代两种不同的类型，那么 A+B 表示两者的和类型，它的值可以是 A 类型的值，也可以是 B 类型的值。

如果我们用逻辑关系做比方，product 可以看作是“与”，sum 可以看作是“或”。

如果抛开构造子，`Term` 其实就是 `Var + Var × Term + Term × Term`。换句话说，`Term` 类型是三种类型的和：它可以是变元、抽象、或应用，三者之一。其中变元等同与 `Var` 类型，抽象等同于一个 `Var` 和一个 `Term` 的积，而应用等同于一个 `Term` 和另一个 `Term` 的积。 

实际上在 C 语言里我们也有 struct 和 union，对定的就是 product 和 sum。这么重要的基础概念，反而在多数脚本语言里得不到支持 (尤其是sum)，实在是遗憾。

除开 product 和 sum，ADT 还有其它几种基本结构：0 (此类型不包含任何值)，1 (此类型只有一个值)，和 → (函数结构，有时也叫做幂)。这样一来称之为 algebraic data type 总算名副其实了吧？这些理论源于 category theory，限于篇幅不再一一讨论。

#习题

一、根据上述 `Term` 和 `Var` 类型的 Haskell 定义，给出以下λ项所对应的 Haskell 表达式：

1. a b c d e f 
1. (g h) (i j) 
1. λx.f (λy.f y) 
1. (λf.f x y) (λx.λy.x) 
1. f g λx.y z

二、Haskell 常用的列表类型 `[a]` 用 product 和 sum 的方式该如何定义？

[BNF]: http://en.wikipedia.org/wiki/Backus–Naur_Form

