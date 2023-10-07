################################################################################
# Makefile for building and running docker containers


# PRODUCTION version -----------------------------------------------------------

production_build:
	docker build --no-cache -t mazamascience/airsensor2:0.3.7 -t mazamascience/airsensor2:latest .

production_run:
	docker run -ti --rm mazamascience/airsensor2 /bin/bash

production_publish:
	docker login && docker push mazamascience/airsensor2:0.3.7

