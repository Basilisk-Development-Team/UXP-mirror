/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef jit_AliasAnalysis_h
#define jit_AliasAnalysis_h

#include "jit/AliasAnalysisShared.h"
#include "jit/MIR.h"
#include "jit/MIRGraph.h"

namespace js {
namespace jit {

class LoopAliasInfo;

class AliasAnalysis : public AliasAnalysisShared
{
    LoopAliasInfo* loop_;

  public:
    AliasAnalysis(MIRGenerator* mir, MIRGraph& graph);
    [[nodiscard]] bool analyze() override;
};

} // namespace jit
} // namespace js

#endif /* jit_AliasAnalysis_h */
