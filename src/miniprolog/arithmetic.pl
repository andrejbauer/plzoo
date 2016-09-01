# Basic arithmetic in miniprolog.

# Numbers are represented in unary:
# z ....... zero
# s(n) .... successor of n

# Addition: plus(K,M,N) means K + M = N
# 0 + N = N
plus(z,N,N).
# s(K) + M = s(K + M)
plus(s(K),M,s(N)) :- plus(K,M,N).

# Subtraction:
minus(K,M,N) :- plus(M,N,K).

# Multiplication: times(K,M,N) means K * M = N
# z * N = z
times(z,N,z).
# (K+1) * M = K * M + M
times(s(K),M,N) :- times(K,M,P), plus(P,M,N).

# Test the program by typing (including "?-"):
# ?- times(s(s(s(z))), s(s(z))).
# ?- plus(X, Y, s(s(s(s(s(z)))))).
