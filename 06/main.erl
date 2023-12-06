-module(main).
-export([main/1]).

read_numbers(Line) ->
    [_ | Tokens] = string:split(string:trim(Line), " ", all),
    [ begin {Int, _ } = string:to_integer(Tok), Int end || Tok <- Tokens, Tok /= "" ].

possibilities(Time, Distance) ->
    % Let t = Time, d = Distance.
    % Let x = the time that someone holds the button to accelerate
    % Let y = the time that it takes to complete a race:
    % y = x * (t - x) = -x^2 + tx
    % Solve: y > d for all x
    % Return: length of x values where this holds
    %
    % y = -x^2 + tx > d
    % -x^2 + tx -d > 0
    % a = -1, b = t, c = -d
    % D = b^2 -4ac = t^2 - 4d
    %
    % Xlo = (-b - sqrt(D)) / 2a = (-t + sqrt(D)) / -2
    % Xhi = (-b + sqrt(D)) / 2a = (-t - sqrt(D)) / -2
    SqrtD = math:sqrt((Time * Time) - 4 * Distance),

    % Define an Epsilon equal to a small number to account for cases where
    % round numbers must be strictly less than, instead of leq.
    Epsilon = 0.0001,
    Xhi = (-Time - SqrtD) / -2 - Epsilon,
    Xlo = (-Time + SqrtD) / -2 + Epsilon,

    trunc(Xhi) - trunc(Xlo).

product ([]) -> 1;
product ([Elem]) -> Elem;
product ([Elem | Rest]) -> Elem * product(Rest).

concat_to_string(Numbers) ->
    {Int, _ } = string:to_integer(lists:concat(lists:map(fun(X) -> lists:flatten(io_lib:format("~w", [X])) end, Numbers))), Int.

main(_) ->
    Times = read_numbers(io:get_line("")),
    Distances = read_numbers(io:get_line("")),

    Possibilities = [ possibilities(Time, Distance) || { Time, Distance } <- lists:zip(Times, Distances) ],
    io:format("part 1: ~w~n", [product(Possibilities)]),

    Time = concat_to_string(Times),
    Distance = concat_to_string(Distances),
    io:format("part 2: ~w~n", [ possibilities(Time, Distance)]).
