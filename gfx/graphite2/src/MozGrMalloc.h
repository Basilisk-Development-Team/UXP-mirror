/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

#ifndef MOZ_GR_MALLOC_H
#define MOZ_GR_MALLOC_H

// Override malloc() and friends to call moz_xmalloc() etc, so that we get
// predictable, safe OOM crashes rather than relying on the code to handle
// allocation failures reliably.

#include "mozilla/mozalloc.h"

#define malloc(a) moz_xmalloc(a)
#define calloc(a, b) moz_xcalloc(a, b)
#define realloc(a, b) moz_xrealloc(a, b)

#endif // MOZ_GR_MALLOC_H
