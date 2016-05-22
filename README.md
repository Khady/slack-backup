# Slack backup

Small OCaml tool to backup channels or conversations from slack.

## Install

`opam install slack-backup`

## Examples

```
$ export SLACK_TOKEN=yoursecrettoken
$ slack-backup list-channels
C04UL8631 -- general
C04UL8637 -- random
$ slack-backup channel --name #general > general.json
$ slack-backup channel --id C04UL8637 > random-with-id.json
$ slack-backup conversation --name slackbot > slackot.json
```

The `token` can also be given with `--token`.

Tokens can be generated from [the slack website](https://api.slack.com/docs/oauth-test-tokens).
There is not yet the support of OAuth in the tool.

More options are described in `slack-backup --help`.

## Requirements

- slacko
- cmdliner
- result

They can be installed using `opam`.
