FROM debian:buster
RUN mkdir -p /opt/inventory-repair-login/
# ARG BINARY_PATH
WORKDIR /opt/inventory-repair-login
RUN apt-get update && apt-get install -y \
  ca-certificates \
  libpq-dev
COPY target /opt/inventory-repair-login
COPY config /opt/inventory-repair-login/config
COPY webroot /opt/inventory-repair-login/webroot
CMD ["/opt/inventory-repair-login/inventory-repair-login"]
