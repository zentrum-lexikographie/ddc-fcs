# DDC-FCS

_API gateway connecting ZDL's text corpora to the Federated Content
Search (FCS) of CLARIN and Text+_

## Prerequisites

* [Docker](https://docs.docker.com/get-docker/): The API gateway is
  implemented in Clojure and is deployed as a Docker container.

## Deployment

    DOCKER_HOST="ssh://clarin.bbaw.de" DDC_FCS_RESTART=unless-stopped\
        docker compose up --build -d

## License

This project is licensed under the GNU Lesser General Public License v3.0.
