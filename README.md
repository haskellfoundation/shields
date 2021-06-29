# `shields` 

## What is this thing?

An implementation of a [Shields.io endpoint](https://shields.io/endpoint) for
The Haskell Foundation.

## What are the goals of this project?

### Simplicity

'Do one thing well' is a key part of this project; it serves as an endpoint,
nothing more, nothing less.

### Low dependencies

Dependencies will only be added if they are necessary. The biggest parts (such
as `snap-core`) are unavoidable, but in general, lighter and smaller solutions
are preferred.

### Clarity

The design, and implementation, will avoid overly-involved solutions. Ideally,
any Haskeller should be able to pick up this project and know what's going on.
This extends to documentation as well.

## How do I use this?

The executable (called `shields`) allows a range of command-line options, which
can be viewed with `--help`. These are listed below:

```
Options:
              --hostname=NAME          local hostname, default "localhost"
  -b ADDRESS  --address=ADDRESS        address to bind to, default "0.0.0.0"
  -p PORT     --port=PORT              port to listen on, default off
              --ssl-address=ADDRESS    ssl address to bind to, default off
              --ssl-port=PORT          ssl port to listen on, default off
              --ssl-cert=PATH          path to ssl certificate in PEM format, default off
              --ssl-chain-cert         certificate file contains complete certificate chain
              --no-ssl-chain-cert      certificate file contains only the site certificate
              --ssl-key=PATH           path to ssl private key in PEM format, default off
              --access-log=PATH        access log, default log to file "log/access.log"
              --error-log=PATH         error log, default log to file "log/error.log"
              --no-access-log          don't have an access log
              --no-error-log           don't have an error log
  -c          --compression            use gzip compression on responses, default compressed
  -t SECS     --timeout=SECS           set default timeout in seconds, default 60
              --no-compression         serve responses uncompressed, default compressed
  -v          --verbose                print server status updates to stderr, default True
  -q          --quiet                  do not print anything to stderr, default verbose
              --proxy=X_Forwarded_For  Set --proxy=X_Forwarded_For if your snap application 
                                       is behind an HTTP reverse proxy to ensure that 
                                       rqClientAddr is set properly.
                                       Set --proxy=haproxy to use the haproxy protocol
                                       (http://haproxy.1wt.eu/download/1.5/doc/proxy-protocol.txt)
              --unix-socket=PATH       Absolute path to unix socket file. File will be removed if already exists
              --unix-socket-mode=MODE  Access mode for unix socket in octal, for example 0760.
                                        Default is system specific.
  -h          --help                   display this help and exit
```

When run, the executable acts as an app server - send requests to it to get
responses.

## What does this run on?

Currently, our CI checks GHC 8.10.4, on each of the following platforms:

* Windows
* Linux
* MacOS

## What can I do with this?

The project is licensed Apache 2.0 (SPDX code
[`Apache-2.0`](https://spdx.org/licenses/Apache-2.0.html)). For more details,
please see the `LICENSE.md` file.

## Who made this?

This work was made with the sponsorship of [MLabs](https://mlabs.city/).
