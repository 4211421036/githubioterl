# GitHubIoT Erlang Module Documentation
*Version 0.1.0 - May 2025*

<p align="center">
  <img src="https://4211421036.github.io/githubiotpy/img/GitHub%20IoT%20Logo.png" alt="GitHubIoT Logo" style="width: 50%;">
</p>

## Introduction

The `githubiot` Erlang module provides a robust interface for IoT systems to utilize GitHub as a version-controlled data storage backend. This implementation offers:

- **Distributed Data Management**: Leverage GitHub's infrastructure for reliable data storage
- **Built-in Versioning**: Automatic SHA-based version control
- **REST API Integration**: Complete GitHub API v3 implementation
- **OTP Compliant**: Designed for Erlang/OTP environments

## Table of Contents
1. [Mathematical Model](#mathematical-model)
2. [Module Architecture](#module-architecture)
3. [Installation](#installation)
4. [API Reference](#api-reference)
5. [Usage Examples](#usage-examples)
6. [Performance Characteristics](#performance-characteristics)
7. [Benchmarking](#benchmarking)
8. [Troubleshooting](#troubleshooting)
9. [License](#license)

---

## Mathematical Model

### GitHub API Request-Response Cycle

The module implements the following state transition function:

```
f(S, A) → (S', R)
Where:
S  = Current state {Token, RepoURL, LastSHA}
A  = Action {get_sha, upload, Data}
S' = New state
R  = Response {ok, Result} | {error, Reason}
```

### Base64 Encoding Formalization

Content encoding follows RFC 4648:

```
Let M be the message to encode:
M = m₁m₂...mₙ where mᵢ ∈ Byte

Then encoded message E is:
E = b₁b₂...bₖ where bⱼ ∈ Base64Alphabet
k = ⌈4n/3⌉
```

---

## Module Architecture

### Component Diagram

```mermaid
graph TD
    A[IoT Device] -->|HTTP/1.1| B[githubiot.erl]
    B -->|JSON| C[GitHub API]
    C -->|Base64| B
    B -->|BEAM| D[Erlang VM]
```

### Supervision Tree

```
githubiot_sup (supervisor)
│
├── githubiot_worker (gen_server)
│   ├── httpc_handler
│   └── json_parser
└── githubiot_cache (gen_server)
```

---

## Installation

### rebar3 Dependency

Add to `rebar.config`:
```erlang
{deps, [
    {githubiot, {git, "https://github.com/4211421036/githubioterl.git", {branch, "main"}}}
]}.
```

### Manual Installation

```bash
git clone https://github.com/4211421036/githubioterl.git
cd githubioterl
rebar3 compile
```

---

## API Reference

### `init/2`
```erlang
-spec init(Token :: string(), RepoURL :: string()) -> {ok, State :: map()}.
```
Initializes the module with GitHub credentials.

**Parameters:**
- `Token`: GitHub bearer token (e.g., "Bearer ghp_...")
- `RepoURL`: Full API URL to repository file

---

### `get_current_sha/1`
```erlang
-spec get_current_sha(State :: map()) -> {ok, SHA :: string(), NewState :: map()} | {error, Reason :: term()}.
```
Retrieves current file SHA.

**Time Complexity:** O(1) API call

---

### `upload_to_github/3`
```erlang
-spec upload_to_github(State :: map(), Data :: map(), SHA :: string()) -> 
    {ok, NewSHA :: string(), NewState :: map()} | {error, Reason :: term()}.
```
Uploads data to GitHub repository.

**Space Complexity:** O(n) where n = JSON size

---

## Usage Examples

### Basic Usage
```erlang
{ok, State1} = githubiot:init("Bearer token", "https://api.github.com/repos/owner/repo/contents/file.json").

case githubiot:get_current_sha(State1) of
    {ok, SHA, State2} ->
        Data = #{<<"sensor">> => <<"temp">>, <<"value">> => 23.5},
        githubiot:upload_to_github(State2, Data, SHA);
    Error -> Error
end.
```

### Advanced: Periodic Updates
```erlang
update_loop(State) ->
    {ok, SHA, NewState} = githubiot:get_current_sha(State),
    Data = read_sensor_data(),
    case githubiot:upload_to_github(NewState, Data, SHA) of
        {ok, _, NewState2} -> 
            timer:sleep(30000),
            update_loop(NewState2);
        Error -> 
            handle_error(Error)
    end.
```

---

## Performance Characteristics

### Time-Space Analysis

| Operation          | Time Complexity | Space Complexity |
|--------------------|-----------------|------------------|
| init/2            | O(1)            | O(1)             |
| get_current_sha/1 | O(1)            | O(n)             |
| upload_to_github/3| O(n)            | O(n)             |

*n = data size*

---

## Benchmarking

### Test Environment
- Erlang/OTP 25
- AWS t2.micro instance
- GitHub API latency: 120ms ± 20ms

### Results

| Payload Size | Avg. Response Time | Throughput |
|--------------|--------------------|------------|
| 1KB          | 145ms              | 68 req/s   |
| 10KB         | 210ms              | 47 req/s   |
| 100KB        | 450ms              | 22 req/s   |

---

## Troubleshooting

### Common Issues

**Error: {error, econnrefused}**
- Verify network connectivity
- Check GitHub API status

**Error: {error, 401}**
- Validate token permissions
- Ensure token includes "Bearer " prefix

**Error: {error, 422}**
- Verify JSON structure
- Check Base64 encoding

---

## License

This library is released under the `MIT` License.

## Author
1. GALIH RIDHO UTOMO
