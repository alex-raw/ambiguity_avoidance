#!/usr/bin/env bash

corpus="${1:-BNC}"
lex_file="${corpus}_lex.tsv"
freq_file="${corpus}_freqs.tsv"

lemma="lemma"
pl=".*NNS.*"
[[ $corpus =~ ^BNC ]] && lemma="hw" && pl=".*NN2.*"

build_lex() {
cqp -c <<-SCRIPT
  $corpus;
  s = [word = ".+[sS]" & pos = "$pl|.*VVZ.*"];
  tabulate s match $lemma;
SCRIPT
}

query() {
cqp -c <<-SCRIPT
  $corpus;
  define \$list < "$lex_file";
  s = [$lemma = \$list];
  group s match pos by match word;
SCRIPT
}

main() {
  build_lex | sed 1d | tr "[:upper:]" "[:lower:]" | sort -u > "$lex_file"
  query | sed 1d > "$freq_file"
}

# cwb-decode -n -C $corpus -P word -P pos -P lemma | head
cwb-scan-corpus -o "${corpus}_tuples.tsv" "$corpus" word $lemma pos
