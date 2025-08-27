%%! -pa _build/default/lib/*/ebin

%%%-------------------------------------------------------------------
%% @doc סקריפט להרצת הדמו הגרפי
%%      הרץ עם: escript run_graphics_demo.erl
%%      או: erl -s run_graphics_demo main -s init stop -noshell
%% @end
%%%-------------------------------------------------------------------

-module(run_graphics_demo).
-export([main/0, main/1]).

main() ->
    main([]).

main(_Args) ->
    io:format("~n"),
    io:format("╔════════════════════════════════════════╗~n"),
    io:format("║   מערכת ניהול משלוחים - ממשק גרפי    ║~n"),
    io:format("║         Delivery System UI Demo        ║~n"),
    io:format("╚════════════════════════════════════════╝~n"),
    io:format("~n"),
    
    %% הפעל את הקוד בתוך Erlang shell
    io:format("טוען מודולים...~n"),
    
    %% קומפל את המודולים אם צריך
    compile_if_needed(),
    
    %% הפעל את הדמו
    io:format("~nמפעיל ממשק גרפי...~n"),
    io:format("=====================================~n~n"),
    
    %% הוראות הרצה
    io:format("להרצת הדמו, הקלד בקונסול:~n"),
    io:format("~n"),
    io:format("1> c(graphics_server).~n"),
    io:format("2> c(graphics_demo_runner).~n"),
    io:format("3> graphics_demo_runner:run().~n"),
    io:format("~n"),
    io:format("לאחר שהחלון נפתח:~n"),
    io:format("- לחץ על 'התחל אנימציה' להתחלת הדמו~n"),
    io:format("- לחיצה שמאלית על שליח/הזמנה תציג מידע~n"),
    io:format("- לחיצה ימנית תוסיף שליח חדש~n"),
    io:format("~n"),
    io:format("פקודות נוספות:~n"),
    io:format("- graphics_demo_runner:test_scenario_1(). % תרחיש יום רגיל~n"),
    io:format("- graphics_demo_runner:test_scenario_2(). % תרחיש עומס~n"),
    io:format("- graphics_demo_runner:add_random_order(). % הוסף הזמנה אקראית~n"),
    io:format("- graphics_demo_runner:stop(). % עצור את המערכת~n"),
    io:format("~n"),
    
    ok.

compile_if_needed() ->
    Files = [
        "apps/delivery_system_ui/src/graphics_server.erl",
        "graphics_demo_runner.erl"
    ],
    
    lists:foreach(fun(File) ->
        case filelib:is_file(File) of
            true ->
                io:format("קומפל ~s...~n", [File]),
                compile:file(File, [debug_info, {outdir, "./"}]);
            false ->
                io:format("קובץ ~s לא נמצא, דלג~n", [File])
        end
    end, Files).