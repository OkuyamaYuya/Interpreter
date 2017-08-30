### Syntax

<img src="https://latex.codecogs.com/gif.latex?\fn_cs&space;\large&space;\\&space;\begin{align*}&space;e&space;&=\lambda\&space;x:&space;t&space;.\&space;e\\&space;&\mid&space;let\&space;x:&space;t&space;=&space;e\&space;in\&space;e\\&space;&\mid&space;let\&space;rec\&space;f\&space;x&space;:&space;t&space;=&space;e\&space;in\&space;e&space;\\&space;&\mid&space;[e1,e2,e3,..]&space;\\&space;&\mid&space;list[n]&space;\end{align*}&space;\\&space;\begin{align*}&space;\tau&space;&=&space;Int&space;\\&space;&\mid&space;Bool\\&space;&\mid&space;List&space;\tau&space;\\&space;&\mid&space;\tau&space;\rightarrow&space;\tau&space;\end{align*}" />

( )[定義のメモ]
( )[
\\
\begin{align*}
e &=\lambda\ x: t .\ e\\
   &\mid  let\ x: t = e\ in\ e\\
   &\mid  let\ rec\ f\ x : t = e\ in\ e \\
   &\mid  [e1,e2,e3,..] \\
   &\mid  list[n]
\end{align*}
\\
\begin{align*}
\tau &= Int \\
       &\mid  Bool\\
       &\mid List \tau \\
       &\mid  \tau \rightarrow \tau
\end{align*}
]
