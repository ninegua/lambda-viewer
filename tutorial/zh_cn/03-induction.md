#手把手教你做λ（三）漂亮打印与数学归纳法

在上一节，我们定义了以下的数据类型，用来在 Haskell 程序里表达一个λ项：

     data Term = Var Var | Lam Var Term | App Term Term 
     data Var  = V String

虽然这种内部表达形式和λ项的结构一致，但它并不利于阅读和书写，而且关于 `Term` 的表达式只能写在程序里面。所以接下来我们要解决的问题，就是如何在内部数据类型和书面的字串之间转换，也即关于λ项的输入和输出的问题。

先讲输出。从数据结构转换成字串输出的过程，有个好听的名字叫 Pretty Print。在 Haskell 里，我们通常可以用 show 函数来做这件事情。例如：`show 123` 的结果是字串 `"123"`，而 `show pi` 的结果是 `"3.141592653589793"`。我们可以对 `Term` 和 `Var` 也自行定义 `show` 函数来做 Pretty Print，比如：

     instance Show Var where
       show (V s) = s

关于 `Var` 类型的 `show` 函数非常简单，直接使用在 `Var` 类型的定义里代表变元名字的那个字串就可以了。而对 `Term` 定义 `show` 函数就略为复杂一点：

     instance Show Term where
       show (Var v)   = show v
       show (Lam x e) = "λ" ++ show x ++ "." ++ show e
       show (App f e) = show f ++ " " ++ show e

这里我们针对 `Term` 类型的每种构造形式分别进行处理：

- 对变元结构直接用 `show v`，因为这里 `v` 是 `Var` 类型，而我们已经定义了 `Var` 类型的 `show` 函数。 
- 对函数结构则用 "λ" 和 "." 符号把型参变元 `x` 和函数体 `e` 连接起来，其中对于 `v` 和 `e` 都使用 `show` 来把它们分别转换成字串；
- 对应用结构则用空格把函数部分 f 和参数部分 e 隔开就好了。

在前一节，我们已经注意到定义 `Term` 类型后两种结构时，也同样用到了 `Term` 类型本身。这种递归结构几乎是以同样的形态出现在 `show` 函数的定义里：例如，为了得到 `Lam x e` 里面子结构 `e` 的字串，我们直接使用 `show e`，用 `show` 函数本身来定义 `show` 就是递归。

我们在中学数学里学习过自然归纳法，它把关于自然数 n 的证明，归结成两种情况：

1. 当 n = 1 时证明命题成立。
2. 假设 n = k 时命题成立，证明 n = k + 1 时命题同样成立。

实际上，自然归纳法借助了自然数的内部结构，也即 

- Base case (基础)：1 是自然数。
- Inductive case (归纳)：假如已知 k 为自然数，那么 k + 1 也是自然数。

同样在 `Term` 的定义里，我们也能够观察到类似的情况：

1. `Var Var` 是基础: 如果 `v :: Var` 那么 `Var v :: Term`。这里尽管我们有一个 `v` 属于 `Var` 类型的前提，但它不是递归。
2. `Lam Var Term` 是归纳：如果 `v :: Var`，`e :: Term`，那么 `Lam v e :: Term`。这里对于 `e` 的类型所作的前提假设是递归。
3. `App Term Term` 也是归纳：如果 `f, e :: Term`，那么 `App f e :: Term`。这里对于 `f` 和 `e` 的类型所作的前提假设是递归。

对数据类型采用 base case 和 inductive case 的定义方式叫做归纳定义 (inductive definition)。我们回过头看关于 `Term` 的 `show` 函数，它的定义同样也是归纳定义：

1. `show (Var v)` 是基础：已知 `show v`，将变元结构的 `Term` 转换为字串。
2. `show (Lam v e)` 是归纳：已知 `show v` 和 `show e`，将函数结构的 `Term` 转换为字串。
3. `show (App f e)` 也是归纳：已知 `show f` 和 `show e`，将应用结构的 `Term` 转换为字串。

作为程序员，我们可能更熟悉遍历 (traversal) 这个概念，把 `show` 函数看作是对一个有着递归结构的数据进行遍历操作，在每个节点计算它所对应的字串。有趣的是，在这里我们又一次成功地把具象的操作和抽象的数学归纳法对应起来了。

而作为细心的程序员的你，一定也早就发现上面定义的 `show` 函数有个 bug：它没有考虑到用字串书写λ项可能会产生的歧义。比如以下两个表达式的计算结果都是字串 `"λx.x x"`：

- show (Lam (V "x") (App (Var (V "x")) (Var (V "x"))))
- show (App (Lam (V "x") (Var (V "x"))) (Var (V "x")))

而它们完全是两种结构。为了消除歧义，我们可以在 Pretty Print 时加入括号。更正后的 `show` 函数如下：

     instance Show Term where
       show (Var v)   = show v
       show (Lam x e) = "(λ"++ show x ++"."++ show e ++")"
       show (App f e) = "(" ++ show f ++" "++ show e ++")"

这样一来，上面两个例子的结果分别是 `"(λx.(x x))"` 和 `"((λx.x) x)"`，就不会产生歧义了。

#习题

一、如果把上面的 `show` 函数看作是对 `Term` 的遍历操作，那它是先序 (pre-order) 还是后序 (post-order)？你能用另一种方法实现 show 函数吗？

二、上面的 `show` 函数作为 Pretty Print 其实还不够漂亮，因为括号用的太多了。比如 (λx.(x x))，考虑到 . 的作用范围是到最右边，可以简写为 λx.x x；而如果是 ((f x) y)，考虑到应用结构是左结合 (left associative)，可以化简为 f x y。你能实现一个新的 `show` 函数，让它的结果包含最少的括号吗？

