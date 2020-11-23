FROM debian:10-slim

RUN apt-get update -y && apt-get install wget unzip -y

RUN wget https://www.dolphindb.cn/downloads/DolphinDB_Linux64_V1.20.7.zip \
    && unzip DolphinDB_Linux64_V1.20.7.zip \
    && cd server \
    && chmod +x dolphindb

WORKDIR server

COPY start.sh .
COPY dolphindb.cfg .

CMD ["./start.sh"]
