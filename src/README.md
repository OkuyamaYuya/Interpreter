### Syntax

\begin{eqnarray}
e &:=& ... \\
  &|& \x:\tau\ .\ e \\

  &|& let\ x:\tau\ =\ e\ in\ e \\

  &|& let\ rec\ (f:\tau)\ x\ =\ e\ in\ e \\ \\

\tau &:=& Int \\
     &|& Bool \\
     &|& \tau \to \tau

\end{eqnarray}