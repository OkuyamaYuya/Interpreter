### Syntax

\begin{eqnarray}
e &::=& ... \\
  &|& lambda\ x:\tau\ .\ e \\

  &|& val\ x:\tau\ =\ e\ in\ e \\

  &|& fun\ (f:\tau)\ x\ =\ e\ in\ e \\ \\

\tau &:=& INT \\
     &|& BOOL \\
     &|& \tau \to \tau

\end{eqnarray}
