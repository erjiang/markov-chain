SC=csc
SCFLAGS=

chain: chain.ss
	$(SC) $(SCFLAGS) chain.ss

one_meelyun_sentences: one_meelyun_sentences.bz2
	bunzip2 one_meelyun_sentences.bz2

one_meelyun_sentences.bz2:
	curl -O "http://d3t3fd87rd28b5.cloudfront.net/one_meelyun_sentences.bz2"
