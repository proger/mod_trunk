# mod_trunk

Trunk HTTP with XMPP by exposing an HTTP endpoint to forward messages to users routed through a mapping table from string aliases to JIDs.

## Prerequisites

- Erlang/OTP 20 or higher
- ejabberd 18.01 or higher

## Installation and Upgrading

```bash
# Substitute ~ for the home directory of the user that runs ejabberd:
cd ~/.ejabberd-modules/sources/
git clone https://github.com/proger/mod_trunk
# ejabberdctl should now list mod_trunk as available:
ejabberdctl modules_available
# Compile and install mod_trunk:
ejabberdctl module_install mod_trunk

# To upgrade, same as uninstall + install:
ejabberdctl module_upgrade mod_trunk
```

## Configuration

Merge the following into your `ejabberd.yml`. TLS configuration is optional.

```yaml
certfiles:
  - "/etc/ssl/localhost.crt"
  - "/etc/ssl/private/localhost.key"

ca_file: "/etc/ssl/certs/ca-bundle.pem"

listen:
  -
    port: 5444
    ip: "::"
    module: ejabberd_http
    request_handlers:
      # ...
      "/trunk": mod_trunk
    tls: true

modules:
  mod_trunk:
    config:
      token: "secret"
      src_host: "sms.example.com"
```

## Usage

Configure `mod_trunk` aliases:

```erlang
mod_trunk:register_alias(<<"+14085503542">>, <<"john@localhost">>).

mod_trunk:all_aliases().
```

Send messages:

```bash
curl -v --cacert /etc/ssl/certs/ca-bundle.pem https://localhost:5444/trunk -d '
  {"src": "TheMatrix",
   "dst": "+14085503542",
   "text": "Follow the white rabbit.",
   "ts": 1520000000,
   "token": "secret"
  }
'
```

## Operation Notes

```
# Module information:
mod_trunk:health().

# Divert logs:
lager:trace_file("/tmp/mod_trunk.log", [{module, mod_trunk}], debug).
```
