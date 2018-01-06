main(Args, Res) :-
    ( _{name: Name} :< Args; Name = 'World' ),
    atomics_to_string(['Hello, ', Name, '!'], Greetings),
    sub(_{payload: Greetings}, Res).

sub(A, A) :- writeln(user_output, 'sub predicate is called').
