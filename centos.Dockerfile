FROM centos:latest
RUN mkdir -p /opt/inventory-repair-login/
# ARG BINARY_PATH
WORKDIR /opt/inventory-repair-login
RUN dnf update -y && dnf install -y \
  ca-certificates \
  postgresql-devel
COPY target /opt/inventory-repair-login
COPY config /opt/inventory-repair-login/config
COPY webroot /opt/inventory-repair-login/webroot
CMD ["/opt/inventory-repair-login/inventory-repair-login"]
