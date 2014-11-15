c4 - C in four functions
========================

*** Hacked by charcole                   ****

*** Added Z-Machine output as experiment ****

clang -m32 -w c4.c -o c4.out && ./c4.out -z sieve.c > test.inf && ~/Downloads/inform6/inform6.out -v3 -y test.inf \$MAX_STATIC_DATA=64000 \$MAX_LABELS=10000 \$MAX_SYMBOLS=64000 > 2.txt && ~/code/zmachine/zops.out test.z3

An exercise in minimalism.

Try the following:

    gcc -o c4 c4.c  (you may need the -m32 option on 64bit machines)
    ./c4 hello.c
    ./c4 -s hello.c
    
    ./c4 c4.c hello.c
    ./c4 c4.c c4.c hello.c

