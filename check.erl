-module(check).

-export([main/1]).


format_prompt(Prompt, Encoding) ->
    lists:flatten(io_lib:format_prompt(Prompt, Encoding)).


skip_banner() ->
    receive
        {io_request, From, ReplyAs,
         {put_chars, unicode, _M, _F, _A}} ->
            From ! {io_reply, ReplyAs, ok}
    end.


consume([], Text) ->
    Text;
consume([H|T1], [H|T2]) ->
    consume(T1, T2).


read_input(Text, Prompt, Module, Function, Continuation, ExtraArgs) ->
    Text1 = consume(format_prompt(Prompt, unicode), Text),
    case Text1 of
        "\n" ->
            {eof, []};
        _ ->
            {Line, Text2} =
                case string:str(Text1, "\n") of
                    0 ->
                        {Text1, []};
                    Index ->
                        {lists:sublist(Text1, 1, Index),
                         lists:nthtail(Index, Text1)}
                end,

            case apply(Module, Function, [Continuation, Line|ExtraArgs]) of
                {done, Result, []} ->
                    {Result, Text2};
                {more, Continuation1} ->
                    read_input(Text2, Prompt, Module, Function, Continuation1, ExtraArgs)
            end
    end.


wait_terminate() ->
    receive
        {io_request, _From, _ReplyAs, {put_chars, unicode, M, F, A}} ->
            "*** Terminating erlang (nonode@nohost)\n" = lists:flatten(apply(M, F, A))
    end.


loop(Text) ->
    receive
        {io_request, From, ReplyAs,
         {get_until, unicode, Prompt, Module, Function, ExtraArgs}} ->
            {Result, Text1} = read_input(Text, Prompt, Module, Function, [], ExtraArgs),
            From ! {io_reply, ReplyAs, Result},
            case Result of
                eof ->
                    wait_terminate();
                _ ->
                    loop(Text1)
            end;
        {io_request, From, ReplyAs, getopts} ->
            From ! {io_reply, ReplyAs, []},
            loop(Text);
        {io_request, From, ReplyAs, {get_geometry, columns}} ->
            From ! {io_reply, ReplyAs, 80},
            loop(Text);
        {io_request, From, ReplyAs, {requests, Requests}} ->
            Text1 =
                lists:foldl(
                  fun ({put_chars, Encoding, Chars}, Txt) ->
                          consume(
                            unicode:characters_to_list(Chars, Encoding),
                            Txt)
                  end,
                  Text,
                  Requests),
            From ! {io_reply, ReplyAs, ok},
            loop(Text1);
        {io_request, From, ReplyAs, {put_chars, unicode, Chars}} ->
            Text1 = consume(unicode:characters_to_list(Chars), Text),
            From ! {io_reply, ReplyAs, ok},
            loop(Text1);
        {io_request, From, ReplyAs, {put_chars, unicode, Module, Function, Args}} ->
            Text1 = consume(unicode:characters_to_list(apply(Module, Function, Args)), Text),
            From ! {io_reply, ReplyAs, ok},
            loop(Text1);
        Data ->
            io:format("Unknown Data: ~p~n~n", [Data])
    end.


start_verify_session(Text) ->
    process_flag(trap_exit, true),
    G = group_leader(),
    group_leader(self(), self()),
    Shell = shell:start(),
    group_leader(G, self()),
    link(Shell),

    skip_banner(),
    loop(Text),
    exit(Shell, kill),
    receive
        {'EXIT', _, killed} ->
            ok
    end.


verify_session(Text) ->
    process_flag(trap_exit, true),
    Pid = spawn_link(fun() -> start_verify_session(Text) end),
    receive
        {'EXIT', From, Reason}
          when From =:= Pid ->
            Reason
    end.


find_all_sessions({<<"pre">>, Attrs, Children}) ->
    case proplists:get_value(<<"class">>, Attrs) of
        undefined ->
            [];
        Class ->
            Classes =
                string:tokens(
                  unicode:characters_to_list(Class), " "),

            case lists:member("erl", Classes) of
                false ->
                    [];
                true ->
                    [$\n|Text] = unicode:characters_to_list(Children),
                    [Text]
            end
    end;
find_all_sessions({_, _, Children}) ->
    lists:append([find_all_sessions(Node) || Node <- Children, not is_binary(Node)]).


verify_sessions(Tree) ->
    Sessions = find_all_sessions(Tree),
    lists:map(
      fun(Session) ->
              {Session, verify_session(Session)}
      end,
      Sessions).


find_all_snippets({<<"pre">>, Attrs, Children}) ->
    case proplists:get_value(<<"class">>, Attrs) of
        undefined ->
            [];
        Class ->
            Classes =
                string:tokens(
                  unicode:characters_to_list(Class), " "),

            case lists:member("erlang", Classes) of
                false ->
                    [];
                true ->
                    [$\n|Text] = unicode:characters_to_list(Children),
                    [Text]
            end
    end;
find_all_snippets({_, _, Children}) ->
    lists:append([find_all_snippets(Node) || Node <- Children, not is_binary(Node)]).


locate_snippet(_Snippet, []) ->
    false;
locate_snippet(Snippet, [{Filename, Text}|Rest]) ->
    case string:str(Text, Snippet) of
        0 ->
            locate_snippet(Snippet, Rest);
        N ->
            Line = string:words(lists:sublist(Snippet, N-1), $\n),
            [{file, Filename}, {line, Line}]
    end.


locate_snippets(Tree, SourceFiles) ->
    Snippets = find_all_snippets(Tree),

    lists:map(
      fun(Snippet) ->
              {Snippet, locate_snippet(Snippet, SourceFiles)}
      end,
      Snippets).


test_files(Filenames) ->
    CompilingResults =
        lists:map(
          fun(Filename) ->
                  Name = lists:sublist(Filename, length(Filename)-4),
                  {Filename, compile:file(Name, [return_errors, report_warnings])}
          end,
          Filenames),

    TestingResults =
        lists:map(
          fun
              ({Filename, {ok, Module}}) ->
                  {ok, {Module, [{exports, Exports}]}} = beam_lib:chunks(Module, [exports]),
                  case lists:member({test, 0}, Exports) of
                      false ->
                          {Filename, {ok, no_test}};
                      true ->
                          case catch(Module:test()) of
                              ok ->
                                  {Filename, {ok, test_success}};
                              {'EXIT', Result} ->
                                  {Filename, {failure, Result}}
                          end
                  end;
              ({Filename, Result}) ->
                  {Filename, Result}
          end,
          CompilingResults),

    [code:purge(Module) || {_, {ok, Module}} <- CompilingResults],
    TestingResults.


find_files(Ext, Dir) ->
    {ok, Filenames} = file:list_dir(Dir),
    [Filename || Filename <- Filenames, lists:suffix(Ext, Filename)].

read_tree(Filename, Dir) ->
    Fullname = lists:flatten([Dir, "/", Filename, ".html"]),
    {ok, Bin} = file:read_file(Fullname),
    {Fullname, mochiweb_html:parse(Bin)}.

read_erl(Filename, Dir) ->
    Fullname = lists:flatten([Dir, "/", Filename]),
    {ok, Bin} = file:read_file(Fullname),
    {Fullname, unicode:characters_to_list(Bin)}.

check_folder(Folder) ->
    {ok, Cwd} = file:get_cwd(),
    CodeFolder = lists:flatten(["code/", Folder]),

    Erls = find_files(".erl", CodeFolder),
    Trees = [read_tree(Folder, "text")],
    SourceFiles = [read_erl(Name, CodeFolder) || Name <- Erls],

    ok = file:set_cwd(CodeFolder),

    CodeResult =
        [ {lists:flatten(["text/", Folder, "/", Name]), Result}
          || {Name, Result} <- test_files(find_files(".erl", "."))],

    TextResult =
        lists:map(
          fun({Fullname, Tree}) ->
                  {Fullname, locate_snippets(Tree, SourceFiles), verify_sessions(Tree)}
          end,
          Trees),

    ok = file:set_cwd(Cwd),
    {TextResult, CodeResult}.


find_folders() ->
    {ok, Cwd} = file:get_cwd(),
    {ok, Names} = file:list_dir(lists:flatten([Cwd, "/code"])),
    Names.


format_exception(Reason, StackTrace) ->
    lib:format_exception(
      1, error, Reason, StackTrace,
      fun(M, _F, _A) ->
              (M =:= erl_eval) or (M =:= ?MODULE)
      end,
      fun(V, I) ->
              io_lib_pretty:print(
                V,
                [{column, I},
                 {line_length, 80},
                 {strings, true},
                 {depth, 30},
                 {max_chars, 60}])
      end,
      unicode).


print_text_results({Fullname, SnippetResults, SessionResults}) ->
    lists:foreach(
      fun
          ({Snippet, false}) ->
              io:format("=SNIPPET NOT FOUND====~nSnippet in ~s not found:~n~s~n", [Fullname, Snippet]);
          (_) ->
              ok
      end,
      SnippetResults),

    lists:foreach(
      fun
          ({_, normal}) ->
              ok;
          ({Session, {Reason, StackTrace}}) ->
              Ex = format_exception(Reason, StackTrace),
              io:format("=SHELL SESSION ERROR====~nError occurs in ~s:~n~s~n~s~n~n", [Fullname, Session, Ex])
      end,
      SessionResults),

    {Fullname,
     [{length([ ok || {_, R} <- SnippetResults, R =/= false]), length(SnippetResults)},
      {length([ok ||{_, normal} <- SessionResults]), length(SessionResults)}]}.


print_code_results({Filename, {error, Errors, Warnings}}) ->
    io:format("=COMPILING ERROR====~ncompilation of ~s failed:~n", [Filename]),
    lists:foreach(
      fun ({File, Desc}) ->
              lists:foreach(
                fun({Line, Module, ErrorDescription}) ->
                        io:format("~s:~w: Error: ~s~n", [File, Line, Module:format_error(ErrorDescription)])
                end,
                Desc)
      end,
      Errors++Warnings),
    io:format("~n"),
    {Filename, [{0,1},{0,1}]};
print_code_results({Filename, {failure, {Reason, StackTrace}}}) ->
    io:format("=TEST FAILURE====~nTest in ~s failed:~n~s~n~n", [Filename, format_exception(Reason, StackTrace)]),
    {Filename, [{1,1},{0,1}]};
print_code_results({Filename, _}) ->
    {Filename, [{1,1},{1,1}]}.


main(_) ->
    Folders = find_folders(),
    {TextResults, CodeResults} =
        lists:unzip(
          lists:map(fun (Folder) -> check_folder(Folder) end, Folders)),

    TextReport =
        lists:map(
          fun(Result) ->
                  print_text_results(Result)
          end,
          lists:append(TextResults)),

    CodeReport =
        lists:map(
          fun(Result) ->
                  print_code_results(Result)
          end,
          lists:append(CodeResults)),

    io:format("=RESULTS====~n"),
    lists:foreach(
      fun({Name, Result}) ->
              io:format(
                "~s: ~s~n",
                [Name,
                 string:join(
                   [ io_lib:format("~w/~w", [Success, Total]) || {Success, Total} <- Result],
                   " ")])
      end,
      TextReport ++ CodeReport).
