FROM alpine:3.15

## ==========================  [ Install Racket ] =========================== ##

## Define default Racket version and variant. The Racket version is of the form
## <major>.<minor>. The variant can be "cs" (Chez Scheme), "bc" (Before Chez) or
## "natipkg" (where external libraries are included in the Racket packages).
##
ARG RACKET_VERSION=8.4
ARG RACKET_VARIANT=cs

## Install Racket. We first install system dependencies: [gcompat] is needed for
## Racket and [ncurses] is needed for the [xrepl] and [expeditor] packages,
## providing the REPL. We then download the installer, run it with the right
## parameters, then remove it. After that, all that remains is to set-up the
## Racket packages and install [expeditor]. See later for a description of the
## arguments to [raco pkg install].
##
RUN apk add --no-cache gcompat ncurses
RUN wget "https://download.racket-lang.org/installers/${RACKET_VERSION}/racket-minimal-${RACKET_VERSION}-x86_64-linux-${RACKET_VARIANT}.sh"
RUN echo 'yes\n1\n' | sh racket-minimal-${RACKET_VERSION}-x86_64-linux-${RACKET_VARIANT}.sh --create-dir --unix-style --dest /usr/
RUN rm racket-minimal-${RACKET_VERSION}-x86_64-linux-${RACKET_VARIANT}.sh
RUN raco setup --no-docs
RUN raco pkg install -i --batch --auto --no-docs expeditor-lib

## =================== [ Install Rosette's Dependencies ] =================== ##

## Work on Rosette's installation within /usr/local. This directory will be
## cleaned up later on so it could be anything.
##
WORKDIR /usr/local/rosette

## Get all the info.rkt files. Trying to install Rosette based only on these
## files would fail, but we can use them to only install dependencies.
##
COPY info.rkt         .
COPY rosette/info.rkt rosette/

## Install only Rosette's dependencies. We have to install the external
## dependencies [libstdc++] and [libgcc] because Z3 needs them at runtime. As
## for the Racket dependencies only, we achieve that in three steps:
##
##   1. We use [raco pkg install --no-setup] to download and register Rosette
##      and all its dependencies without setting them up, that is without
##      compiling them. At this point, the system is in an inconsistent state,
##      where packages are registered but not actually present. The other flags
##      are the following:
##
##        -i         install packages for all users
##        --batch    disable interactive mode and suppress prompts
##        --auto     download missing packages automatically
##
##   2. We use [raco pkg remove --no-setup] to unregister Rosette. This keeps
##      the dependencies as registered. The system is still in an inconsistent
##      state. See above for the flags.
##
##   3. We use [raco setup] to set up all the registered package. This brings
##      the system back in a consistent state. Since Rosette's dependencies were
##      registered but not Rosette itself, this achieves our goal. The flags are
##      the following:
##
##        --fail-fast    fail on the first error encountered
##        --no-docs      do not compile the documentations
##
RUN apk add --no-cache libstdc++ libgcc
RUN raco pkg install -i --batch --auto --no-setup ../rosette
RUN raco pkg remove  -i                --no-setup    rosette
RUN raco setup       --fail-fast --no-docs

## ========================== [ Install Rosette ] =========================== ##

## Get all of Rosette; build and install it. The dependencies should all be
## installed, so we can remove the --auto flag which will lead us to failure if
## a dependency cannot be found. The additional flags are the following:
##
##   --copy    copy content to install path (instead of linking)
##
COPY . .
RUN raco pkg install -i --batch --copy --no-docs ./rosette
RUN rm -R /usr/local/rosette

## ===================== [ Prepare Clean Entry Point ] ====================== ##

## For further use of the image, we can start with user `rosette`, group
## `rosette` in `/rosette` by default.
##
RUN addgroup rosette
RUN adduser --system --shell /bin/false --disabled-password \
    --home /rosette --ingroup rosette rosette
RUN chown -R rosette:rosette /rosette
USER rosette
WORKDIR /rosette

## Rosette files are simply Racket files using the Rosette library: the default
## entry point of this image is therefore the Racket executable.
##
ENTRYPOINT ["/usr/bin/racket", "-I", "rosette"]
