Create a .merlin:

  $ cat > .merlin <<EOF
  > S ../../../../src/frontend
  > EOF

  $ echo | $MERLIN single check-configuration -filename test.ml
  {
    "class": "return",
    "value": {
      "dot_merlins": [
        "$TESTCASE_ROOT/.merlin"
      ],
      "failures": []
    },
    "notifications": []
  }
