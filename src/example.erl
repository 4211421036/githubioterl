-module(example).
-export([main/0]).

main() ->
    {ok, State1} = githubiot:init("Bearer ${{ secrets.GITHUB_TOKEN }}", 
                                 "https://api.github.com/repos/${{ github.repository }}/contents/sensor.json"),
    
    case githubiot:get_current_sha(State1) of
        {ok, Sha, State2} ->
            io:format("Current SHA: ~p~n", [Sha]),
            
            JsonData = #{<<"sensor">> => <<"temperature">>, <<"value">> => 25.5},
            case githubiot:upload_to_github(State2, JsonData, Sha) of
                {ok, NewSha, _State3} ->
                    io:format("Upload successful. New SHA: ~p~n", [NewSha]);
                {error, Reason, _} ->
                    io:format("Upload failed: ~p~n", [Reason])
            end;
        {error, Reason, _} ->
            io:format("Failed to get SHA: ~p~n", [Reason])
    end.
