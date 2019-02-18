allyamls := $(shell find . -type f -name '*.yaml')

shared := $(wildcard ucglib/*.ucg)
bucketucgs := $(wildcard buckets/*.ucg)

%.yaml: %.ucg .libtarget
	ucg build $<

# This ensures that we rebuild if the dependent ucglibs
# change.
.libtarget: $(shared)
	touch .libtarget 

build: bucketconfs gensite

bucketconfs: .libtarget $(patsubst %.ucg,%.yaml,$(bucketucgs))

all: build test

test:
	ucg test -r

content := $(shell find site/content -type f -iname *.md)

site/.built: $(content)
	cd site; \
	zola build -o generated
	touch site/.built

gensite: site/.built

deploysite: gensite
	cd site/; \
	gsutil -m rsync -d -r generated gs://ucg.marzhillstudios.com/

clean:
	rm -f $(allyamls) site/.built
	rm -rf site/generated/*
