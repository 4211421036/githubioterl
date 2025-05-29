-module(githubiot).
-export([init/2, get_current_sha/1, upload_to_github/3]).

%% @doc Initialize the module with token and repo URL
-spec init(string(), string()) -> map().
init(Token, RepoUrl) ->
    #{
        token => Token,
        repo_url => RepoUrl,
        last_sha => ""
    }.

%% @doc Get current SHA of the file in GitHub repository
-spec get_current_sha(map()) -> {ok, string(), map()} | {error, term(), map()}.
get_current_sha(State = #{token := Token, repo_url := RepoUrl}) ->
    Headers = [
        {"Authorization", Token},
        {"User-Agent", "Erlang GitHubIoT"}
    ],
    
    case httpc:request(get, {RepoUrl, Headers}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            case jsx:decode(list_to_binary(Body)) of
                #{<<"sha">> := Sha} ->
                    NewState = State#{last_sha => binary_to_list(Sha)},
                    {ok, binary_to_list(Sha), NewState};
                _ ->
                    {error, invalid_json_response, State}
            end;
        {ok, {{_, Status, _}, _, _}} ->
            {error, {http_error, Status}, State};
        {error, Reason} ->
            {error, Reason, State}
    end.

%% @doc Upload data to GitHub repository
-spec upload_to_github(map(), map(), string()) -> {ok, string(), map()} | {error, term(), map()}.
upload_to_sha(State = #{token := Token, repo_url := RepoUrl}, JsonData, LastSha) ->
    Headers = [
        {"Authorization", Token},
        {"Content-Type", "application/json"},
        {"User-Agent", "Erlang GitHubIoT"}
    ],
    
    JsonStr = jsx:encode(JsonData),
    EncodedData = base64:encode(JsonStr),
    
    Payload = jsx:encode(#{
        <<"message">> => <<"Update data">>,
        <<"content">> => list_to_binary(EncodedData),
        <<"sha">> => list_to_binary(LastSha)
    }),
    
    case httpc:request(put, {RepoUrl, Headers, "application/json", Payload}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            case jsx:decode(list_to_binary(Body)) of
                #{<<"content">> := #{<<"sha">> := NewSha}} ->
                    NewState = State#{last_sha => binary_to_list(NewSha)},
                    {ok, binary_to_list(NewSha), NewState};
                _ ->
                    {error, invalid_json_response, State}
            end;
        {ok, {{_, Status, _}, _, _}} ->
            {error, {http_error, Status}, State};
        {error, Reason} ->
            {error, Reason, State}
    end.
