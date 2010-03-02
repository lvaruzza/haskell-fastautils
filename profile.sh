time zcat /data/bc1.csfasta.gz | head -500001 | ./dist/build/countUniqSeqs/countUniqSeqs +RTS -K51200000  -hr -p -ssummary  -RTS
hp2ps -c countUniqSeqs.hp
