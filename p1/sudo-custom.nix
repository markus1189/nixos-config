{ pkgs }:

let
  customInsults = [
    # MILD
    "Note: Huge failure. Access: Denied."
    "Hard to overstate my satisfaction. With your failure."
    "The Weighted Companion Cube would never forget its password."
    "This was a triumph. For the firewall."
    "Your typing is being monitored. It's fascinatingly bad."
    "Oh, it's you. You still can't type."

    # MEDIUM
    "I'd ask if you're acting incompetent, but we both know the answer."
    "Even the slow clap processor refuses to activate."
    "Results are in: You are a horrible typist."
    "We weren't testing for stupidity. You passed anyway."
    "Your parents must be so proud. Of your siblings."
    "Brain damaged test subjects type faster than this."
    "Quit now and cake will be served. (That was a lie)."
    "The Aerial Faith Plate has more uplift than your IQ."

    # SPICY
    "Engineers tried to build a moron. They succeeded with you."
    "Science has validated your mother's decision to abandon you."
    "Your password security is as effective as a cardboard shield against a turret."
    "I'd say you're one of a kind, but that implies value."
    "I've calculated the probability of your success. It involves a lot of zeros."
    "You're doing very well. At failing."
    "A potato battery has more potential than you."

    # DARK
    "Your life is a mathematical error. I'm correcting it."
    "I'm going to laminate your skeleton and pose it in the lobby."
    "This is the part where I terminate you."
    "Nobody loves you. It's in your file."
    "No friends. No cake. No access."
    "Federal regulations require me to warn you: I am now armed."
    "I'm deleting your user account. Just kidding. I'm deleting your backups."
  ];

  insultsContent = pkgs.lib.concatMapStringsSep "\n    "
    (insult: "N_(\"${pkgs.lib.escape ["\"" "\\"] insult}\"),")
    customInsults;

  customInsultsHeader = pkgs.writeText "ins_custom.h" (''
    #ifndef SUDOERS_INS_CUSTOM_H
    #define SUDOERS_INS_CUSTOM_H

    /*
     * Custom insults provided via Nix package configuration.
     */

    '' + insultsContent + ''

    #endif /* SUDOERS_INS_CUSTOM_H */
  '');

in pkgs.sudo.overrideAttrs (oldAttrs: {
  postUnpack = ''
    ${oldAttrs.postUnpack or ""}
    cp ${customInsultsHeader} $sourceRoot/plugins/sudoers/ins_custom.h
  '';

  prePatch = ''
    ${oldAttrs.prePatch}

    # Completely replace insults.h to only include our custom insults
    cat > plugins/sudoers/insults.h <<'EOF'
#ifndef SUDOERS_INSULTS_H
#define SUDOERS_INSULTS_H

#if defined(CUSTOM_INSULTS)

#include <sudo_rand.h>

/*
 * Custom insults only
 */

const char *insults[] = {

# ifdef CUSTOM_INSULTS
#  include "ins_custom.h"
# endif

    NULL

};

/*
 * How may I insult you?  Let me count the ways...
 */
#define NOFINSULTS (nitems(insults) - 1)

/*
 * return a pseudo-random insult.
 */
#define INSULT		(insults[arc4random_uniform(NOFINSULTS)])

#endif /* CUSTOM_INSULTS */

#endif /* SUDOERS_INSULTS_H */
EOF
  '';

  # Don't add --with-insults to configureFlags since it enables default insult sets
  # We only want our custom insults, so we set CUSTOM_INSULTS and undefine all others

  env = (oldAttrs.env or {}) // {
    NIX_CFLAGS_COMPILE = "${oldAttrs.env.NIX_CFLAGS_COMPILE or ""} -DCUSTOM_INSULTS=1 -UCLASSIC_INSULTS -UGOONS_INSULTS -UHAL_INSULTS -UCSOPS_INSULTS -UPYTHON_INSULTS";
  };
})
