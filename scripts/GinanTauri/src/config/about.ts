/**
 * Static facts shown on the Settings → About page.
 *
 * Everything here is taken from the repository rather than written fresh:
 * README.md and Docs/ for what Ginan is, LICENSE.md for the licence, and each
 * bundled package's own LICENSE file for the third-party notices. Keep it that
 * way — if a fact changes, change it at its source and copy it here.
 */

export const ABOUT_LINKS = {
  /** Ginan's repository, by Geoscience Australia; this GUI is part of it. */
  repo: "https://github.com/GeoscienceAustralia/ginan",
  /** Ginan's online manual, as linked from README.md. */
  docs: "https://geoscienceaustralia.github.io/ginan/",
  /** Upstream issue tracker, as linked from README.md. */
  issues: "https://github.com/GeoscienceAustralia/ginan/issues",
  license: "https://www.apache.org/licenses/LICENSE-2.0",
} as const;

/** From LICENSE.md at the repository root. */
export const PROJECT_COPYRIGHT = "Copyright 2020 Geoscience Australia";
export const PROJECT_LICENSE = "Apache License 2.0";

/**
 * Third-party code compiled into the frontend bundle that ships inside the app.
 *
 * Only runtime dependencies belong here — build tools (Vite, TypeScript, the
 * Tauri CLI) are not distributed. These licences (MIT, BSD-2-Clause) require
 * their copyright and permission notice to accompany binary redistributions,
 * which is why the full texts are reproduced below rather than just linked.
 */
export type ThirdPartyNotice = {
  name: string;
  /** SPDX expression, as declared in the package's package.json. */
  license: string;
  copyright: string;
  url: string;
  /** Which entry of `LICENSE_TEXTS` holds the licence body. */
  text: keyof typeof LICENSE_TEXTS;
};

export const THIRD_PARTY_NOTICES: ThirdPartyNotice[] = [
  {
    name: "React, React DOM and Scheduler",
    license: "MIT",
    copyright: "Copyright (c) Facebook, Inc. and its affiliates.",
    url: "https://github.com/react/react",
    text: "MIT",
  },
  {
    name: "use-sync-external-store",
    license: "MIT",
    copyright: "Copyright (c) Meta Platforms, Inc. and affiliates.",
    url: "https://github.com/react/react/tree/main/packages/use-sync-external-store",
    text: "MIT",
  },
  {
    name: "Zustand",
    license: "MIT",
    copyright: "Copyright (c) 2019 Paul Henschel",
    url: "https://github.com/pmndrs/zustand",
    text: "MIT",
  },
  {
    name: "Leaflet",
    license: "BSD-2-Clause",
    copyright:
      "Copyright (c) 2010-2023, Volodymyr Agafonkin\nCopyright (c) 2010-2011, CloudMade",
    url: "https://github.com/Leaflet/Leaflet",
    text: "BSD-2-Clause",
  },
  {
    name: "Tauri JavaScript API and plugins",
    license: "Apache-2.0 OR MIT",
    copyright:
      "Copyright (c) 2017 - Present Tauri Apps Contributors\n" +
      "Copyright (c) 2019-2022, The Tauri Programme in the Commons Conservancy",
    url: "https://github.com/tauri-apps/tauri",
    text: "MIT",
  },
];

/**
 * Licence bodies, verbatim from the packages' LICENSE files minus the title and
 * copyright line, which are printed per component above each body.
 */
export const LICENSE_TEXTS = {
  MIT: `Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.`,
  "BSD-2-Clause": `All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.`,
} as const;
