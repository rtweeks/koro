#!/bin/bash

stack --stack-yaml "$(dirname "$0")/oro-parser/stack.yaml" run -- "$@"
