-module(main).
-export([main/1]).

read_numbers(Line) ->
    [_ | Tokens] = string:split(string:trim(Line), " ", all),
    [ begin {Int, _ } = string:to_integer(Tok), Int end || Tok <- Tokens, Tok /= "" ].

print_elements([]) -> ok;
print_elements([Elem]) -> io:format("~p", [Elem]);
print_elements([ Elem | Rest]) ->
    io:format("~p ", [ Elem ]),
    print_elements(Rest).

possibilities(Time, Distance) ->
    % Let t = Time, d = Distance.
    % Let x = the time that someone holds the button to accelerate
    % Let y = the time that it takes to complete a race:
    % y = x * (t - x) = -x^2 + tx
    % Solve: y > d for all x
    % Return: length of x values where this holds


main(_) ->
    Times = read_numbers(io:get_line("")),
    Distances = read_numbers(io:get_line("")),

    Possibilities = [ possibilities(Time, Distance) || { Time, Distance } <- lists:zip(Times, Distances) ],
    print_elements(Possibilities).
