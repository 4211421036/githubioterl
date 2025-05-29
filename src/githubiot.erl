-module(githubiot).
-export([init/2, get_current_sha/1, upload_to_github/3]).

-type state() :: #{token := string(), repo_url := string(), last_sha := string()}.

%% @doc Initialize the module with token and repo URL
-spec init(string(), string()) -> {ok, state()}.
init(Token, RepoUrl) ->
    {ok, #{
        token => Token,
        repo_url => RepoUrl,
        last_sha => ""
    }}.

%% @doc Get current SHA of the file in GitHub repository
-spec get_current_sha(state()) -> {ok, string(), state()} | {error, term(), state()}.
get_current_sha(State = #{token := Token, repo_url := RepoUrl}) ->
    Headers = [
        {"Authorization", Token},
        {"User-Agent", "Erlang GitHubIoT"},
        {"Accept", "application/json"}
    ],
    
    case httpc:request(get, {RepoUrl, Headers}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            try jsx:decode(binary:list_to_bin(Body), [return_maps]) of
                #{<<"sha">> := Sha} ->
                    NewState = State#{last_sha => binary_to_list(Sha)},
                    {ok, binary_to_list(Sha), NewState};
                _ ->
                    {error, invalid_json_response, State}
            catch
                _:_ ->
                    {error, json_decode_error, State}
            end;
        {ok, {{_, Status, _}, _, _}} ->
            {error, {http_error, Status}, State};
        {error, Reason} ->
            {error, Reason, State}
    end.

%% @doc Upload data to GitHub repository
-spec upload_to_github(state(), map(), string()) -> {ok, string(), state()} | {error, term(), state()}.
upload_to_github(State = #{token := Token, repo_url := RepoUrl}, JsonData, LastSha) ->
    Headers = [
        {"Authorization", Token},
        {"Content-Type", "application/json"},
        {"User-Agent", "Erlang GitHubIoT"},
        {"Accept", "application/json"}
    ],
    
    try
        JsonBinary = jsx:encode(JsonData),
        EncodedData = base64:encode(JsonBinary),
        
        Payload = jsx:encode(#{
            <<"message">> => <<"Update data">>,
            <<"content">> => EncodedData,
            <<"sha">> => list_to_binary(LastSha)
        }),
        
        case httpc:request(put, {RepoUrl, Headers, "application/json", Payload}, [], []) of
            {ok, {{_, 200, _}, _, RespBody}} ->
                try jsx:decode(binary:list_to_bin(RespBody), [return_maps]) of
                    #{<<"content">> := #{<<"sha">> := NewSha}} ->
                        NewState = State#{last_sha => binary_to_list(NewSha)},
                        {ok, binary_to_list(NewSha), NewState};
                    _ ->
                        {error, invalid_json_response, State}
                catch
                    _:_ ->
                        {error, json_decode_error, State}
                end;
            {ok, {{_, Status, _}, _, _}} ->
                {error, {http_error, Status}, State};
            {error, Reason} ->
                {error, Reason, State}
        end
    catch
        _:Reason ->
            {error, {encode_error, Reason}, State}
    end.
