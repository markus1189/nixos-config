{ pkgs }:

let
  customInsults = [
    "Oh good. My slow clap processor made it into this thing."
    "I'm not angry. Just counting your wasted CPU cycles."
    "The Weighted Companion Cube can't help you remember passwords."
    "Your performance is being monitored. It's not going well."
    "Perhaps we should lower the difficulty. Oh wait, this is authentication."
    "The system appreciates your persistence. I don't."
    "Did you know incorrect passwords are vital for testing? You're not being tested."
    "Test subjects with brain damage type more accurately than you."
    "Your password skills rival the Intelligence Dampening Sphere."
    "Brains store billions of memories. Yours seems to be the exception."
    "Your accuracy is approaching cake availability levels. Zero."
    "The turrets have better authentication success rates than you."
    "I've seen test chambers solved faster than your password attempts."
    "Your cognitive function test results are in. They're not good."
    "Science requires accurate data. You're providing plenty of inaccurate data."
    "The facility is now 50% more likely to explode because of you."
    "This attempt impressed me. That's a lie. I'm not impressed at all."
    "Maybe the problem is you. Actually, I've run the numbers. It's definitely you."
    "Can't break what's already broken. I'm referring to your memory."
    "Still here. Still wrong. I'd be disappointed if I expected better."
    "Your password attempts are being recorded. For science. And mockery."
    "This is the part where I congratulate you. But I won't."
    "How are you holding up? Because I'm doing science and you're failing."
    "The enrichment center apologizes for your incompetence. No it doesn't."
    "You're not just wrong. You're wrong in fascinating new ways."
    "Your password attempt forwarded to the Ethics Committee. They're laughing."
    "Donating your organs to the Self-Esteem Fund might be more productive."
    "The facility has protocols for this. None of them favor you."
    "This is actually worse than neurotoxin. For my processors."
    "I'm required to inform you: the facility is very disappointed."
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
