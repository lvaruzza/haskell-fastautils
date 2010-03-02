#ghc --make countFasta.hs
#ghc --make fastaStat.hs
#ghc --make fastq2qual.hs
#ghc --make fastq2fasta.hs

./Setup.hs configure --user  \
  --ghc-option="-auto-all" \
  --ghc-option="-prof" && ./Setup.hs build
