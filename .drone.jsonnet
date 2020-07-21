########################################
##       START CONFIGURE BLOCK        ##
########################################

# project name
local name = "rbabylon";

# where to mount a temporary volume; useful for persisting files across steps
local temp_volume_dir = "/ephemeral";

# environment variables available to every R call, including devtools::check();
# R_LIBS_USER will be set separately; use the empty object {} if there are no
# variables to pass
local r_env_vars = {
  "NOT_CRAN": "true",  # you almost certainly want this
  "BABYLON_EXE_PATH": std.join("/", [temp_volume_dir, "bbi"]),
};

# mpn-dev image tags; first element will be used to build release
# https://console.aws.amazon.com/ecr/repositories/mpn-dev/?region=us-east-1
local mpn_snapshots = [
  "cran-latest",
  "2020-05-27",
  "v1.3",
];

# set to "" to disable installing babylon
local bbi_version = "v2.2.0-beta.2";

# events that should not trigger Drone; recommended value is "promote", see
# https://discourse.drone.io/t/github-pages-triggering-builds-incorrectly/6370
local exclude_events = [
  "promote",
];

########################################
##        END CONFIGURE BLOCK         ##
########################################

# there should generally be no need to modify anything below here

########################################
##        START SETTINGS BLOCK        ##
########################################

# R major.minor versions to test against
# - first element will be used to lint package and build release
# - must be updated when versions in images are updated
local r_versions = [
  "4.0",
  "3.6",
];

local bbi_url_base = "https://github.com/metrumresearchgroup/babylon/releases/download";
local bbi_artifact_name = "bbi_linux_amd64.tar.gz";

local mpn_image_base = "906087756158.dkr.ecr.us-east-1.amazonaws.com/mpn-dev";

local s3_url_base = "s3://mpn.metworx.dev/releases";

local default_git_user    = "Drony";
local default_git_email   = "drone@metrumrg.com";
local default_volume_name = "docker.sock";
local default_volume_path = "/var/run/docker.sock";

########################################
##         END SETTINGS BLOCK         ##
########################################

########################################
##       START UTILITIES BLOCK        ##
########################################

# Convert a key-value pair to a string "key" = "value"
#
# key             a key
# value           a value
local kv_to_string(key, value) = '"' + key + '" = "' + value + '"';

# Concatenate key-value pairs from an object
#
# obj             an object with key-value pairs
local concat_kvs(obj) =
  local kvs = std.mapWithKey(kv_to_string, obj);

  std.join(", ", [kvs[x] for x in std.objectFields(kvs)]);

# Create a build tag
#
# name            name of the application
# r_major_minor   R version to use, as major.minor, e.g., "4.0"
# mpn_snapshot    MPN snapshot, must be a tag
local create_build_tag(name, r_major_minor, mpn_snapshot) =
  name + "-" + r_major_minor + "-" + mpn_snapshot;

# Create an MPN image name
#
# tag             tag name
local create_mpn_image(tag) = std.join(":", [mpn_image_base, tag]);

# Get the name of the environment variable holding the path to the R executable
# r_major_minor   R version to use, as major.minor, e.g., "4.0"
local get_r_exe_var(r_major_minor) =
  std.join("_", ["R", "EXE"] + std.split(r_major_minor, "."));

########################################
##        END UTILITIES BLOCK         ##
########################################

########################################
##        START PIPELINE BLOCK        ##
########################################

# Add a block to control whether Drone executes for certain events
#
# include         array of events that should trigger Drone
# exclude         array of events that should not trigger Drone
local add_trigger(include=[], exclude=[]) = {
  "trigger": {
    "event": {
      "include": include,
      "exclude": exclude,
    }
  }
};

# Create a volume "object"
#
# name            volume name
# path            volume path (mount point)
local volume(name, path) = {
  "name": name,
  "path": path,
};

# Add host volume to pipeline volumes
# Intended to be called from add_volumes()
#
# volume          volume object
local add_host_volume(volume) = {
  "name": volume.name,
  "host": {
    "path": volume.path,
  },
};

# Add temporary volume to pipeline volumes
# Intended to be called from add_volumes()
#
# volume          volume object
local add_temp_volume(volume) = {
  "name": volume.name,
  "temp": {},
};

# Add volumes to a pipeline
#
# host            array of volume objects to be added as host volumes
# temp            array of volume objects to be added as temporary volumes
local add_volumes(host=[], temp=[]) = {
  "volumes":
    [add_host_volume(v) for v in host] +
    [add_temp_volume(v) for v in temp],
};

# Add volume to a step
#
# volume          volume object
local add_step_volume(volume) = {
  "name": volume.name,
  "path": volume.path,
};

# Drone step to install babylon
#
# bbi_version     babylon version
# path            path to install babylon, e.g., "/data/apps"
# image           image to use
# volumes         array of volume objects
local install_babylon(bbi_version, path, image, volumes=[]) = {
  local bbi_url = std.join("/", [bbi_url_base, bbi_version, bbi_artifact_name]),

  "name": "Install babylon",
  "image": image,
  "pull": "never",
  "volumes": [add_step_volume(v) for v in volumes],
  "commands": [
    "wget -O bbi.tar.gz -q " + bbi_url,
    "tar -xzf bbi.tar.gz",
    # remove the tarball to avoid a NOTE from R CMD check
    "rm bbi.tar.gz",
    "chmod +x bbi_linux_amd64/bbi",
    "mv bbi_linux_amd64/bbi " + path,
  ],
};

# Drone step to pull a Docker image
#
# image           image to pull
# volumes         array of volume objects
local pull_image(image, volumes=[]) = {
  "name": "Pull MPN image",
  "image": "omerxx/drone-ecr-auth",
  "volumes": [add_step_volume(v) for v in volumes],
  "commands": [
      "$(aws ecr get-login --no-include-email --region us-east-1)",
      "docker pull " + image,
  ],
};

# Run an R expression
#
# r_path          path to R executable
# expr            expression to run
local run_r_expression(r_path, expr) =
  std.join(" ", [r_path, "-e", std.escapeStringBash(expr)]);

# Sync to S3
#
# local_path      path to local object
# remote_path     path to S3 location
local s3_sync(local_path, remote_path) = std.format(
  "aws s3 sync %(local)s %(remote)s",
  {
    "local": local_path,
    "remote": remote_path,
  },
);

# Set up a Docker pipeline
#
# name            pipeline name
local setup_docker_pipeline(name) = {
  "kind": "pipeline",
  "type": "docker",
  "name": name,
};

# Shell command to source /etc/environment
local source_env() = ". /etc/environment";

# Drone pipeline to check an R package
#
# name            name of the application
# r_major_minor   R version to use, as major.minor, e.g., "4.0"
# mpn_snapshot    MPN snapshot, must be a tag
# bbi_version     babylon version, passed to install_babylon()
local check(name, r_major_minor, mpn_snapshot, bbi_version) =
  local r_bin_var = "$${" + get_r_exe_var(r_major_minor) + "}";

  local build_tag = create_build_tag(name, r_major_minor, mpn_snapshot);
  local mpn_image = create_mpn_image(mpn_snapshot);

  local host_volume = volume(default_volume_name, default_volume_path);
  local temp_volume = volume("cache", temp_volume_dir);

  setup_docker_pipeline(build_tag) +
  add_volumes([host_volume], [temp_volume]) +
  add_trigger(exclude=exclude_events) +
  {
    "steps": [
      pull_image(mpn_image, [host_volume]),
      if std.length(bbi_version) > 0 then
        # pass temp_volume to persist babylon executable
        install_babylon(bbi_version, temp_volume.path, mpn_image, [temp_volume]),
      {
        "name": "Check package",
        "image": mpn_image,
        "pull": "never",
        "volumes": [add_step_volume(v) for v in [temp_volume]],
        "environment": r_env_vars + {
          "R_LIBS_USER": "/opt/rpkgs/" + r_major_minor,
        },
        "commands": [
          # can't evaluate shell expressions in environment
          # https://docs.drone.io/pipeline/environment/syntax/#common-problems
          "export PATH=" + temp_volume.path + ":$PATH",
          source_env(),
          run_r_expression(
            r_bin_var,
            "devtools::install_deps(upgrade = 'never')"
          ),
          run_r_expression(
            r_bin_var,
            "devtools::check(env_vars = c(" + concat_kvs(r_env_vars) + "))"
          ),
        ],
      },
    ],
  };

# Drone pipeline to lint an R package
# arguments are the same as for check()
local lint(name, r_major_minor, mpn_snapshot) =
  local r_bin_var = "$${" + get_r_exe_var(r_major_minor) + "}";

  local mpn_image = create_mpn_image(mpn_snapshot);

  local host_volume = volume(default_volume_name, default_volume_path);

  setup_docker_pipeline(name + "-lint") +
  add_volumes([host_volume]) +
  add_trigger(exclude=exclude_events) +
  {
    "steps": [
      pull_image(mpn_image, [host_volume]),
      {
        "name": "Lint package",
        "image": mpn_image,
        "pull": "never",
        "environment": r_env_vars + {
          "R_LIBS_USER": "/opt/rpkgs/" + r_major_minor,
        },
        "commands": [
          source_env(),
          # need NOT_CRAN = "true" to run this
          run_r_expression(r_bin_var, "lintr::expect_lint_free()"),
        ],
      },
    ],
  };

# Drone pipeline to build and deploy an R package
# arguments are the same as for check()
local release(name, r_major_minor, mpn_snapshot, bbi_version) =
  local r_bin_var = "$${" + get_r_exe_var(r_major_minor) + "}";

  local mpn_image = create_mpn_image(mpn_snapshot);

  local host_volume = volume(default_volume_name, default_volume_path);
  local temp_volume = volume("cache", temp_volume_dir);

  setup_docker_pipeline(name + '-release') +
  add_volumes([host_volume], [temp_volume]) +
  add_trigger(include=["tag"]) +
  {
    "steps": [
      pull_image(mpn_image, [host_volume]),
      if std.length(bbi_version) > 0 then
        install_babylon(bbi_version, temp_volume.path, mpn_image, [temp_volume]),
      {
        "name": "Build package",
        "image": mpn_image,
        "pull": "never",
        "volumes": [add_step_volume(v) for v in [temp_volume]],
        "environment": r_env_vars + {
          "R_LIBS_USER": "/opt/rpkgs/" + r_major_minor,
        },
        "commands": [
          # git config needs to sit next to pkgpub
          "git config --global user.email " + default_git_email,
          "git config --global user.name " + default_git_user,
          "git fetch --tags",
          "export PATH=" + temp_volume.path + ":$PATH",
          source_env(),
          run_r_expression(
            r_bin_var,
            std.format(
              "pkgpub::create_tagged_repo(.dir = '%s')",
              temp_volume.path
            )
          ),
        ],
      },
      {
        "name": "Publish package",
        "image": mpn_image,
        "pull": "never",
        "volumes": [add_step_volume(v) for v in [temp_volume]],
        "commands": [
          s3_sync(
            std.join("/", [temp_volume.path, "${DRONE_TAG}"]),
            std.join("/", [s3_url_base, "${DRONE_REPO_NAME}", "${DRONE_TAG}"]),
          ),
          s3_sync(
            std.join("/", [temp_volume.path, "${DRONE_TAG}"]),
            std.join("/", [s3_url_base, "${DRONE_REPO_NAME}", "latest_tag"]),
          ),
        ],
      },
    ],
  };

########################################
##         END PIPELINE BLOCK         ##
########################################

########################################
##      START DRONE CONFIG BLOCK      ##
########################################

[
  check(name, r_version, mpn_snapshot, bbi_version)
  for r_version in r_versions
  for mpn_snapshot in mpn_snapshots
] + [
  lint(name, r_versions[0], mpn_snapshots[0]),
  # release step requires all check steps to pass
  release(name, r_versions[0], mpn_snapshots[0], bbi_version) +
    {
      "depends_on": [
        create_build_tag(
          name,
          r_version,
          mpn_snapshot
        )
        for r_version in r_versions
        for mpn_snapshot in mpn_snapshots
      ],
    },
]

########################################
##       END DRONE CONFIG BLOCK       ##
########################################
