10 DIM Word_1$(20), Word_2$(20), DLDm(21, 21)
11 CLS
20 Word_1$="kitten" : Word_2$="sitting" : ? Word_1$;" - ";Word_2$ : EXEC _DLD_ : ?
30 Word_1$="rosettacode" : Word_2$="raisethysword" : ? Word_1$;" - ";Word_2$ : EXEC _DLD_ : ?
40 Word_1$="qwerty" : Word_2$="qweryt" : ? Word_1$;" - ";Word_2$ : EXEC _DLD_ : ?
50 Word_1$="MARTHA" : Word_2$="MARHTA" : ? Word_1$;" - ";Word_2$ : EXEC _DLD_ : ?
60 Word_1$="DIXON" : Word_2$="DICKSONX" : ? Word_1$;" - ";Word_2$ : EXEC _DLD_ : ?
70 Word_1$="JELLYFISH" : Word_2$="SMELLYFISH" : ? Word_1$;" - ";Word_2$ : EXEC _DLD_ : ?

11000 END
11600 PROC _DLD_ : REM DamerauLevenshteinDistance
11601   REM Word_1$ : Word_2$ : DLDm[]
11602   I=0: J=0 : K=0 : L=0 : M=0 : N=0 : Min=0 : Result=0

11610   M=LEN(Word_1$) : N=LEN(Word_2$)
11620   FOR I=0 TO M : DLDm(I,0)=I : NEXT I
11630   FOR J=0 TO N : DLDm(0,J)=J : NEXT J
11640   FOR J=1 TO N
11650     FOR I=1 TO M
11660       IF Word_1$(I,I) = Word_2$(J,J)
11670         DLDm(I,J) = DLDm(I-1, J-1) : REM no operation required
11680       ELSE
11690         Min = DLDm(I-1, J)+1 : REM delete
11700         K = DLDm(I, J-1)+1   : REM insert
11710         L = DLDm(I-1, J-1)+1 : REM substitution
11720         IF K < Min THEN Min=K
11730         IF L < Min THEN Min=L
11740         DLDm(I,J) = Min

11750         IF I>1 AND J>1 : REM ---> Damerau Levenshtein Distance BEGIN
11760           IF Word_1$(I,I) = Word_2$(J-1,J-1) AND Word_1$(I-1,I-1) = Word_2$(J,J)
11770               Min=DLDm(I,J) : IF Min>(DLDm(I-2,J-2)+1) THEN Min=(DLDm(I-2,J-2)+1)
11780               DLDm(I,J) = Min : REM transposition
11790           ENDIF
11800         ENDIF : REM <--- Damerau Levenshtein Distance END

11810       ENDIF
11820     NEXT I
11830   NEXT J
11840   Result=DLDm(M,N)
11845   ? "Damerau Levenshtein Distance=";Result
11850 ENDPROC
