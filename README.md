# dolphinc

The DolphinDB erlang API implement

Supported DolphinDB version:
- 1.20 +

## Roadmap

- [x] Connect/ Login
- [x] Script Support
- [ ] Data Parser/ Function/ Variable api support
- [ ] API2 Support
- [ ] Integeration Tests
- [ ] Benchmark

## Usage

``` erlang
1> {ok, C} = dolphinc:start_link([{host, "127.0.0.1"}, {username, "admin"}, {password, "123456"}]).
{ok,<0.155.0>}
2>
2> dolphinc:run(C, <<"1+1">>).
{ok,<<4,0,2,0,0,0>>,[]}
```

## License

EMQ X Team.

## References

- [api-protocol.md](https://github.com/dolphindb/Tutorials_CN/blob/master/api_protocol.md)
- [dolphondb/api-java](https://github.com/dolphindb/api-java)
