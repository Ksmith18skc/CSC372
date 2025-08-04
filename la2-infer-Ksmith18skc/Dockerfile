# To create a new docker container:
#    docker build -t prolog_sml .

FROM ubuntu:22.04

# Update package lists and install Python 3 and pip
RUN apt update && apt install -y python3 python3-pip

# Verify installation
RUN python3 --version && pip3 --version

# Set Python 3 as the default python (optional)
RUN ln -s /usr/bin/python3 /usr/bin/python

# Update package lists and install dependencies for SWI-Prolog and PolyML
RUN apt-get update && apt-get install -y \
    swi-prolog \
    polyml \
    && apt-get clean

# Set up a non-root user (optional)
RUN useradd -ms /bin/bash devuser
USER devuser
WORKDIR /home/devuser

# Verify installations
RUN poly -version && \
    swipl --version

# Set default command
CMD ["/bin/bash"]
