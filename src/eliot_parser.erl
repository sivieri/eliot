% Module skeleton from: http://forum.trapexit.org/viewtopic.php?p=20260

-module(eliot_parser).
-export([parse_transform/2, function/5]).
-define(ERROR(R, T, F, I),
    begin
        rpt_error(R, T, F, I),
        throw({error,erl_syntax:get_pos(
               proplists:get_value(form,I)),{unknown,R}})
    end).
-define(CHANGED, [{{erlang, send}, {eliot_simulator, send}},
                               {{erlang, send_after}, {eliot_simulator, send_after}},
                               {{erlang, register}, {eliot_simulator, register}},
                               {{erlang, spawn}, {eliot_simulator, spawn}},
                               {{erlang, spawn_link}, {eliot_simulator, spawn_link}},
                               {{erlang, export}, {eliot_simulator, export}},
                               {{erlang, unexport}, {eliot_simulator, unexport}}]).
-define(CHANGED2, {eliot_simulator, send}).

% Public API

parse_transform(Forms, Options) ->
    lists:foldl(fun({{StartModule, StartFunction}, {EndModule, EndFunction}}, AccIn) ->
        parse_function(AccIn, Options, StartModule, StartFunction, EndModule, EndFunction) end, Forms, ?CHANGED).

function({_Module, _Function} = MF, F1, F2, Forms, Options) when is_function(F1) andalso is_function(F2) ->
    parse_transform(MF, F1, F2, Forms, Options).

% Private API

parse_function(Forms, Options, StartModule, StartFunction, EndModule, EndFunction) ->
    function({StartModule, StartFunction},
         fun(Form, _Context) ->
             erl_syntax:application(erl_syntax:module_qualifier(erl_syntax:atom(EndModule), erl_syntax:atom(EndFunction)),
                                    erl_syntax:application_arguments(Form))
         end,
         fun(Form, _Context) ->
             case erl_syntax:operator_name(erl_syntax:infix_expr_operator(Form)) of
                '!' ->
                    {Module, Function} = ?CHANGED2,
                    erl_syntax:application(erl_syntax:module_qualifier(erl_syntax:atom(Module), erl_syntax:atom(Function)),
                                    [erl_syntax:infix_expr_left(Form), erl_syntax:infix_expr_right(Form)]);
                 '~' ->
                    {Module, Function} = ?CHANGED2,
                    erl_syntax:application(erl_syntax:module_qualifier(erl_syntax:atom(Module), erl_syntax:atom(Function)),
                                    [erl_syntax:infix_expr_left(Form), erl_syntax:infix_expr_right(Form)]);
                _Any ->
                    Form
             end
         end, Forms, Options).

parse_transform(MF, Fun, Fun2, Forms, _Options) ->
    [File|_] = [F || {attribute,_,file,{F,_}} <- Forms],
    try begin
        NewTree = xform(MF, Fun, Fun2, Forms, [{file, File}]),
        revert_tree(NewTree)
    end
    catch
    throw:{error,Ln,What} ->
        {error, [{File, [{Ln,?MODULE,What}]}], []}
    end.

revert_tree(Tree) ->
    [erl_syntax:revert(T) || T <- lists:flatten(Tree)].

xform({M,F}, Fun, Fun2, Forms, Context0) ->
    Bef = fun(function, Form, Ctxt) ->
          {Fname, Arity} = erl_syntax_lib:analyze_function(Form),
          VarNames = erl_syntax_lib:new_variable_names(
                   Arity,
                   erl_syntax_lib:variables(Form)),
          {Form, [{function, Fname},
              {arity, Arity},
              {var_names, VarNames}|Ctxt]};
         (_, Form, Context) ->
          {Form, Context}
      end,
    Aft = fun(application, Form, Context) ->
          case erl_syntax_lib:analyze_application(Form) of
              {M, {F, _A}} ->
              erl_syntax:add_ann(
                bind_state,
                Fun(Form, Context));
              {F, _A} ->
              erl_syntax:add_ann(
                bind_state,
                Fun(Form, Context));
              _ ->
              Form
          end;
         (function, Form, Context) ->
          Form1 =
              erl_syntax_lib:map_subtrees(
            fun(Clause) ->
                case should_i_bind(Clause) of
                    true ->
                    Pats = erl_syntax:clause_patterns(Clause),
                    CBod = erl_syntax:clause_body(Clause),
                    CGd = erl_syntax:clause_guard(Clause),
                    Pats1 =
                        lists:zipwith(
                          fun(V, P) ->
                              erl_syntax:match_expr(erl_syntax:variable(V), P)
                          end,
                          proplists:get_value(
                        var_names, Context),
                          Pats),
                    erl_syntax:clause(Pats1, CGd, CBod);
                    false ->
                    Clause
                end
            end, Form),
          Form1;
         (infix_expr, Form, Context) ->
          Fun2(Form, Context);
         (_, Form, _Context) ->
          Form
      end,
    [Module] = [Mx || {attribute, _, module, Mx} <- Forms],
    transform(Forms, Bef, Aft, [{module, Module}|Context0]).

transform(Forms, Before, After, Context) ->
    F1 =
    fun(Form) ->
        Type = erl_syntax:type(Form),
        {Form1, Context1} =
            try Before(Type, Form, Context)
            catch
            error:Reason ->
                ?ERROR(Reason, 'before', Before, 
                   [{type, Type},
                    {context, Context},
                    {form, Form}])
            end,
        Form2 =
            case erl_syntax:subtrees(Form1) of
            [] ->
                Form1;
            List ->
                NewList =
                transform(
                  List, Before, After, Context1),
                erl_syntax:update_tree(Form, NewList)
            end,
        Type2 = erl_syntax:type(Form2),
        try After(Type2, Form2, Context1)
        catch
            error:Reason2 ->
            ?ERROR(Reason2, 'after', After, 
                   [{type, Type2},
                {context, Context1},
                {form, Form2}])
        end
    end,
    F2 = fun(List) when is_list(List) ->
         map(F1, List);
        (Form) ->
         F1(Form)
     end,
    map(F2, Forms).

map(F, [Hd|Tail]) ->
    {Before, Res, After} =
    case F(Hd) of
        {Be, _, Af} = Result when is_list(Be), is_list(Af) ->
        Result;
        R1 ->
        {[], R1, []}
    end,
    Rs = map(F, Tail),
    Before ++ [Res| After ++ Rs];
map(F, []) when is_function(F, 1) -> [].

rpt_error(Reason, BeforeOrAfter, Fun, Info) ->
    Fmt = lists:flatten(
        ["*** ERROR in parse_transform function:~n"
         "*** Reason     = ~p~n"
         "*** applying ~w fun (~p)~n",
         ["*** ~10w = ~p~n" || _ <- Info]]),
    Args = [Reason, BeforeOrAfter, Fun | 
        lists:foldr(
          fun({K,V}, Acc) ->
              [K, V | Acc]
          end, [], Info)],
    io:format(Fmt, Args).

should_i_bind(Tree) ->
    erl_syntax_lib:fold(
      fun(T, Flag) ->
          lists:member(bind_state, erl_syntax:get_ann(T)) or Flag
      end, false, Tree).
