FROM clojure:temurin-21-alpine

RUN mkdir -p /ddc-fcs
WORKDIR /ddc-fcs

COPY deps.edn /ddc-fcs/
RUN clojure -P

COPY ./ /ddc-fcs

ENTRYPOINT ["clojure", "-J-XX:+UseContainerSupport", "-J-XX:MaxRAMPercentage=50", "-X", "ddc.fcs/serve"]
