-module(githubiot).
-export([init/2, get_current_sha/1, upload_to_github/3]).

-type state() :: #{token := binary(), repo_url := binary(), last_sha := binary()}.

%% @doc Initialize the module with token and repo URL
-spec init(binary(), binary()) -> {ok, state()}.
init(Token, RepoUrl) ->
    {ok, #{
        token => Token,
        repo_url => RepoUrl,
        last_sha => <<>>
    }}.

%% @doc Get current SHA of the file in GitHub repository
-spec get_current_sha(state()) -> {ok, binary(), state()} | {error, term(), state()}.
get_current_sha(State = #{token := Token, repo_url := RepoUrl}) ->
    Headers = [
        {"Authorization", binary_to_list(Token)},
        {"User-Agent", "Erlang GitHubIoT"},
        {"Accept", "application/json"}
    ],
    
    case httpc:request(get, {binary_to_list(RepoUrl), Headers}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            try jsx:decode(list_to_binary(Body), [return_maps]) of
                #{<<"sha">> := Sha} ->
                    NewState = State#{last_sha => Sha},
                    {ok, Sha, NewState};
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
-spec upload_to_github(state(), map(), binary()) -> {ok, binary(), state()} | {error, term(), state()}.
upload_to_github(State = #{token := Token, repo_url := RepoUrl, last_sha := LastSha}, JsonData, _) ->
    Headers = [
        {"Authorization", binary_to_list(Token)},
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
            <<"sha">> => LastSha
        }),
        
        case httpc:request(put, {binary_to_list(RepoUrl), Headers, "application/json", Payload}, [], []) of
            {ok, {{_, 200, _}, _, RespBody}} ->
                try jsx:decode(list_to_binary(RespBody), [return_maps]) of
                    #{<<"content">> := #{<<"sha">> := NewSha}} ->
                        NewState = State#{last_sha => NewSha},
                        {ok, NewSha, NewState};
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
        _:_ ->
            {error, encode_error, State}
    end.
