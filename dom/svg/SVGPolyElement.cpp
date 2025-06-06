/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#include "SVGPolyElement.h"
#include "DOMSVGPointList.h"
#include "mozilla/gfx/2D.h"
#include "SVGContentUtils.h"

using namespace mozilla;
using namespace mozilla::gfx;

//----------------------------------------------------------------------
// nsISupports methods

NS_IMPL_ADDREF_INHERITED(SVGPolyElement,SVGPolyElementBase)
NS_IMPL_RELEASE_INHERITED(SVGPolyElement,SVGPolyElementBase)

NS_INTERFACE_MAP_BEGIN(SVGPolyElement)
NS_INTERFACE_MAP_END_INHERITING(SVGPolyElementBase)

//----------------------------------------------------------------------
// Implementation

SVGPolyElement::SVGPolyElement(already_AddRefed<mozilla::dom::NodeInfo>& aNodeInfo)
  : SVGPolyElementBase(aNodeInfo)
{
}

SVGPolyElement::~SVGPolyElement()
{
}

already_AddRefed<DOMSVGPointList>
SVGPolyElement::Points()
{
  void *key = mPoints.GetBaseValKey();
  RefPtr<DOMSVGPointList> points = DOMSVGPointList::GetDOMWrapper(key, this, false);
  return points.forget();
}

already_AddRefed<DOMSVGPointList>
SVGPolyElement::AnimatedPoints()
{
  void *key = mPoints.GetAnimValKey();
  RefPtr<DOMSVGPointList> points = DOMSVGPointList::GetDOMWrapper(key, this, true);
  return points.forget();
}


//----------------------------------------------------------------------
// nsIContent methods

NS_IMETHODIMP_(bool)
SVGPolyElement::IsAttributeMapped(const nsIAtom* name) const
{
  static const MappedAttributeEntry* const map[] = {
    sMarkersMap
  };
  
  return FindAttributeDependence(name, map) ||
    SVGPolyElementBase::IsAttributeMapped(name);
}

//----------------------------------------------------------------------
// nsSVGElement methods

/* virtual */ bool
SVGPolyElement::HasValidDimensions() const
{
  return !mPoints.GetAnimValue().IsEmpty();
}

//----------------------------------------------------------------------
// SVGGeometryElement methods

bool
SVGPolyElement::AttributeDefinesGeometry(const nsIAtom *aName)
{
  if (aName == nsGkAtoms::points)
    return true;

  return false;
}

void
SVGPolyElement::GetMarkPoints(nsTArray<nsSVGMark> *aMarks)
{
  const SVGPointList &points = mPoints.GetAnimValue();

  if (!points.Length())
    return;

  float px = points[0].mX, py = points[0].mY, prevAngle = 0.0;

  aMarks->AppendElement(nsSVGMark(px, py, 0, nsSVGMark::eStart));

  for (uint32_t i = 1; i < points.Length(); ++i) {
    float x = points[i].mX;
    float y = points[i].mY;
    float angle = atan2(y-py, x-px);

    // Vertex marker.
    if (i == 1) {
      aMarks->ElementAt(0).angle = angle;
    } else {
      aMarks->ElementAt(aMarks->Length() - 1).angle =
        SVGContentUtils::AngleBisect(prevAngle, angle);
    }

    aMarks->AppendElement(nsSVGMark(x, y, 0, nsSVGMark::eMid));

    prevAngle = angle;
    px = x;
    py = y;
  }

  aMarks->LastElement().angle = prevAngle;
  aMarks->LastElement().type = nsSVGMark::eEnd;
}

bool
SVGPolyElement::GetGeometryBounds(Rect* aBounds,
                                  const StrokeOptions& aStrokeOptions,
                                  const Matrix& aToBoundsSpace,
                                  const Matrix* aToNonScalingStrokeSpace)
{
  const SVGPointList &points = mPoints.GetAnimValue();

  if (!points.Length()) {
    // Rendering of the element is disabled
    aBounds->SetEmpty();
    return true;
  }

  if (aStrokeOptions.mLineWidth > 0 || aToNonScalingStrokeSpace) {
    // We don't handle non-scaling-stroke or stroke-miterlimit etc. yet
    return false;
  }

  if (aToBoundsSpace.IsRectilinear()) {
    // We can avoid transforming each point and just transform the result.
    // Important for large point lists.
    Rect bounds(points[0], Size());
    for (uint32_t i = 1; i < points.Length(); ++i) {
      bounds.ExpandToEnclose(points[i]);
    }
    *aBounds = aToBoundsSpace.TransformBounds(bounds);
  } else {
    *aBounds = Rect(aToBoundsSpace.TransformPoint(points[0]), Size());
    for (uint32_t i = 1; i < points.Length(); ++i) {
      aBounds->ExpandToEnclose(aToBoundsSpace.TransformPoint(points[i]));
    }
  }
  return true;
}

