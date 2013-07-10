#手把手教你做λ（四）语法识别器、高阶函数和列表推导

上一节里我们实现了把 `Term` 类型转换为字串的 `show` 函数，解决了输出问题。接下来，我们要反过来，考虑该如何把一个代表λ项的字串转换为 `Term` 类型的数据结构，这也叫做语法识别器 (parser)。在 Haskell 的标准库里，有一个跟 `Show` 对应的类型类 (type class)，叫做 `Read`：

      type ReadS a = String -> [(a, String)]
      class Read a where
        readsPrec :: Int -> ReadS a

类型 `ReadS a` 所代表的就是对类型 `a` 的识别器，它是这样一个函数：对给定的字串参数从左到右进行识别 (parse)，如果成功，则返回所识别的值 (属于 `a` 类型) 和剩下的字串；如果不成功，则返回空列表。通常成功时只返回一个值，但是考虑到有时候需要允许歧义，用列表也可以返回多个值，以代表所有的可能性。

而定义类型类 `Read` 的实例 (instance) 则需要定义一个 `readsPrec` 函数，它的参数代表优先级，返回的则是 `ReadS a` 所代表的识别器。我们这里用不到优先级，可以忽略掉。主要任务是实现类型 `Var` 和类型 `Term` 的识别器。

假设λ表达式里所有的变元都是单个字母，那么很容易就可以写出如下的识别器：

      import Data.Char

      variable :: ReadS Var
      variable (c:s) | isAlpha c = [(V [c], s)]
      variable _                 = []

      instance Read Var where
        readsPrec _ = variable   

这里我们用到了模式匹配来判断字串的第一个字符是否是字母（`isAlpha` 函数出自 Data.Char 模块），如果是，则成功返回符合 `Var` 类型的值 `V [c]` 和余下的字串 `s`。否则返回空列表 `[]`，代表识别失败。其中 `| isAlpha c = ...` 的写法叫做 `guard`，意思是满足条件 `isAlpha c == True` 的情况下才计算右边。这是一种语法糖 (syntactic sugar），意即非关键的语法性能，它可以完全转成 case 的写法。

实际上，我们会经常用到单个字符的识别器，所以这个功能可以单独被提取出来，成为下面的函数：

      char :: (Char -> Bool) -> ReadS Char
      char f (c:s) | f c = [(c, s)]
      char f _           = []

这样一来，用 `char isAlpha` 就可以识别所有的字母。细心的读者会发现，`char isAlpha` 是 `ReadS Char` 类型，那怎么把它变成我们需要的 `ReadS Var` 类型呢？根据 `ReadS` 的定义，其返回的值是一个列表，那么事情就简单了：

      variable s = map f (char isAlpha s)
        where f (c, s1) = (V [c], s1)

注意到 `char isAlpha s` 返回的是 `[(Char,String)]` 类型，我们要把它变成 `variable s` 所需要返回的 `[(Var, String)]` 类型，而这可以用 `map :: (a -> b) -> [a] -> [b]` 来做到。如果 `f` 是从类型 `a` 到类型 `b` 的映射，那么 `map f` 则是从类型 `[a]` 到类型 `[b]`。换句话说，`map` 是一个高阶函数 (higher-order function)，它能够把一个 `a -> b` 类型的函数，转换 `[a] -> [b]` 类型的函数，用于处理列表。

关于列表的运算，除了使用 `map` 这样的函数，在 Haskell 里还可以用推导式 (comprehension) 来表示，比如 `variable` 还可以写成：

      variable s = [(V [c], s1) | (v, s1) <- char isAlpha s]

列表推导式也是一种语法糖，上述两种 `variable` 函数的写法完全等价，而使用推导式有时能够帮助理解复杂的表达式。这很类似于数学里关于集合运算的写法，比如上面这个定义就可以理解为：对于列表 `char isAlpha s` 里的每一个元素 `(v, s1)`，生成新的元素 `(V [c], s1)` 并组成新的列表返回。

假设我们规定变元必须是两个字母，那么该如何实现这样的识别器？这就需要用到复杂一点的列表推导式：

      variable2 s = [ (V [c1,c2], s2) 
                    | (c1, s1) <- char isAlpha s 
                    , (c2, s2) <- char isAlpha s1 ]

这可以简单理解为，在 `s` 字串中识别一个字母 `c1`，在剩下的 `s1` 字串中识别第二个字母 `c2`，将两个字母拼成一个字串 `[c1,c2]` 成为识别结果。

# 习题

一、在不使用 GHC 或 GHCi 的情况下，(心算)给出下列表达式的值：

1. variable ""
1. variable "a"
1. variable "123"
1. variable "abc"
1. variable2 ""
1. variable2 "a" 
1. variable2 "abc" 

二、把 variable2 用普通方法定义，不要使用列表推导。如果要更挑战一点，可以考虑仅用 `map` 和 `concat`。

三、定义一个函数 `variableN :: Int -> ReadS Var`，根据参数 n 生成长度为 n 的变元识别器。仅考虑 n >= 0 的情况。

