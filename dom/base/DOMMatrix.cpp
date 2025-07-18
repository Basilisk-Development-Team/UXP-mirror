/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#include "mozilla/dom/BindingUtils.h"
#include "mozilla/dom/DOMMatrixBinding.h"
#include "mozilla/dom/DOMPointBinding.h"
#include "mozilla/dom/BindingDeclarations.h"
#include "mozilla/dom/ToJSValue.h"

#include "mozilla/dom/DOMPoint.h"
#include "mozilla/dom/DOMMatrix.h"

#include "SVGTransformListParser.h"
#include "SVGTransform.h"

#include <math.h>

#include "js/Equality.h"  // JS::SameValueZero

namespace mozilla {
namespace dom {

template <typename T>
static void
SetDataInMatrix(DOMMatrixReadOnly* aMatrix, const T* aData, int aLength, ErrorResult& aRv);

static const double radPerDegree = 2.0 * M_PI / 360.0;

NS_IMPL_CYCLE_COLLECTION_WRAPPERCACHE(DOMMatrixReadOnly, mParent)

NS_IMPL_CYCLE_COLLECTION_ROOT_NATIVE(DOMMatrixReadOnly, AddRef)
NS_IMPL_CYCLE_COLLECTION_UNROOT_NATIVE(DOMMatrixReadOnly, Release)

JSObject*
DOMMatrixReadOnly::WrapObject(JSContext* aCx, JS::Handle<JSObject*> aGivenProto)
{
  return DOMMatrixReadOnlyBinding::Wrap(aCx, this, aGivenProto);
}

// https://drafts.fxtf.org/geometry/#matrix-validate-and-fixup-2d
static bool
ValidateAndFixupMatrix2DInit(DOMMatrix2DInit& aMatrixInit, ErrorResult& aRv)
{
#define ValidateAliases(field, alias, fieldName, aliasName)             \
  if ((field).WasPassed() && (alias).WasPassed() &&                     \
      !JS::SameValueZero((field).Value(), (alias).Value())) {           \
    aRv.ThrowTypeError<MSG_MATRIX_INIT_CONFLICTING_VALUE>((fieldName),  \
                                                          (aliasName)); \
    return false;                                                       \
  }
#define SetFromAliasOrDefault(field, alias, defaultValue) \
  if (!(field).WasPassed()) {                             \
    if ((alias).WasPassed()) {                            \
      (field).Construct((alias).Value());                 \
    } else {                                              \
      (field).Construct(defaultValue);                    \
    }                                                     \
  }
#define ValidateAndSet(field, alias, fieldName, aliasName, defaultValue) \
  ValidateAliases((field), (alias), NS_LITERAL_STRING(fieldName),        \
                  NS_LITERAL_STRING(aliasName));                         \
  SetFromAliasOrDefault((field), (alias), (defaultValue));

  ValidateAndSet(aMatrixInit.mM11, aMatrixInit.mA, "m11", "a", 1);
  ValidateAndSet(aMatrixInit.mM12, aMatrixInit.mB, "m12", "b", 0);
  ValidateAndSet(aMatrixInit.mM21, aMatrixInit.mC, "m21", "c", 0);
  ValidateAndSet(aMatrixInit.mM22, aMatrixInit.mD, "m22", "d", 1);
  ValidateAndSet(aMatrixInit.mM41, aMatrixInit.mE, "m41", "e", 0);
  ValidateAndSet(aMatrixInit.mM42, aMatrixInit.mF, "m42", "f", 0);

  return true;

#undef ValidateAliases
#undef SetFromAliasOrDefault
#undef ValidateAndSet
}

// https://drafts.fxtf.org/geometry/#matrix-validate-and-fixup
static bool
ValidateAndFixupMatrixInit(DOMMatrixInit& aMatrixInit, ErrorResult& aRv)
{
#define Check3DField(field, fieldName, defaultValue)  \
  if ((field) != (defaultValue)) {                    \
    if (!aMatrixInit.mIs2D.WasPassed()) {             \
      aMatrixInit.mIs2D.Construct(false);             \
      return true;                                    \
    }                                                 \
    if (aMatrixInit.mIs2D.Value()) {                  \
      aRv.ThrowTypeError<MSG_MATRIX_INIT_EXCEEDS_2D>( \
          NS_LITERAL_STRING(fieldName));              \
      return false;                                   \
    }                                                 \
  }

  if (!ValidateAndFixupMatrix2DInit(aMatrixInit, aRv)) {
    return false;
  }

  Check3DField(aMatrixInit.mM13, "m13", 0);
  Check3DField(aMatrixInit.mM14, "m14", 0);
  Check3DField(aMatrixInit.mM23, "m23", 0);
  Check3DField(aMatrixInit.mM24, "m24", 0);
  Check3DField(aMatrixInit.mM31, "m31", 0);
  Check3DField(aMatrixInit.mM32, "m32", 0);
  Check3DField(aMatrixInit.mM34, "m34", 0);
  Check3DField(aMatrixInit.mM43, "m43", 0);
  Check3DField(aMatrixInit.mM33, "m33", 1);
  Check3DField(aMatrixInit.mM44, "m44", 1);

  if (!aMatrixInit.mIs2D.WasPassed()) {
    aMatrixInit.mIs2D.Construct(true);
  }
  return true;

#undef Check3DField
}

void
DOMMatrixReadOnly::SetDataFromMatrix2DInit(const DOMMatrix2DInit& aMatrixInit) {
  MOZ_ASSERT(Is2D());
  mMatrix2D->_11 = aMatrixInit.mM11.Value();
  mMatrix2D->_12 = aMatrixInit.mM12.Value();
  mMatrix2D->_21 = aMatrixInit.mM21.Value();
  mMatrix2D->_22 = aMatrixInit.mM22.Value();
  mMatrix2D->_31 = aMatrixInit.mM41.Value();
  mMatrix2D->_32 = aMatrixInit.mM42.Value();
}

void
DOMMatrixReadOnly::SetDataFromMatrixInit(const DOMMatrixInit& aMatrixInit)
{
  const bool is2D = aMatrixInit.mIs2D.Value();
  MOZ_ASSERT(is2D == Is2D());
  if (is2D) {
    SetDataFromMatrix2DInit(aMatrixInit);
  } else {
    mMatrix3D->_11 = aMatrixInit.mM11.Value();
    mMatrix3D->_12 = aMatrixInit.mM12.Value();
    mMatrix3D->_13 = aMatrixInit.mM13;
    mMatrix3D->_14 = aMatrixInit.mM14;
    mMatrix3D->_21 = aMatrixInit.mM21.Value();
    mMatrix3D->_22 = aMatrixInit.mM22.Value();
    mMatrix3D->_23 = aMatrixInit.mM23;
    mMatrix3D->_24 = aMatrixInit.mM24;
    mMatrix3D->_31 = aMatrixInit.mM31;
    mMatrix3D->_32 = aMatrixInit.mM32;
    mMatrix3D->_33 = aMatrixInit.mM33;
    mMatrix3D->_34 = aMatrixInit.mM34;
    mMatrix3D->_41 = aMatrixInit.mM41.Value();
    mMatrix3D->_42 = aMatrixInit.mM42.Value();
    mMatrix3D->_43 = aMatrixInit.mM43;
    mMatrix3D->_44 = aMatrixInit.mM44;
  }
}

already_AddRefed<DOMMatrixReadOnly> DOMMatrixReadOnly::FromMatrix(
    nsISupports* aParent, const DOMMatrix2DInit& aMatrixInit,
    ErrorResult& aRv) {
  DOMMatrix2DInit matrixInit(aMatrixInit);
  if (!ValidateAndFixupMatrix2DInit(matrixInit, aRv)) {
    return nullptr;
  };

  RefPtr<DOMMatrixReadOnly> matrix =
      new DOMMatrixReadOnly(aParent, /* is2D */ true);
  matrix->SetDataFromMatrix2DInit(matrixInit);
  return matrix.forget();
}

already_AddRefed<DOMMatrixReadOnly> DOMMatrixReadOnly::FromMatrix(
    nsISupports* aParent, const DOMMatrixInit& aMatrixInit, ErrorResult& aRv) {
  DOMMatrixInit matrixInit(aMatrixInit);
  if (!ValidateAndFixupMatrixInit(matrixInit, aRv)) {
    return nullptr;
  };

  RefPtr<DOMMatrixReadOnly> rval =
      new DOMMatrixReadOnly(aParent, matrixInit.mIs2D.Value());
  rval->SetDataFromMatrixInit(matrixInit);
  return rval.forget();

}

already_AddRefed<DOMMatrixReadOnly>
DOMMatrixReadOnly::FromMatrix(const GlobalObject& aGlobal, const DOMMatrixInit& aMatrixInit, ErrorResult& aRv)
{
  DOMMatrixInit matrixInit(aMatrixInit);
  if (!ValidateAndFixupMatrixInit(matrixInit, aRv)) {
    return nullptr;
  };

  RefPtr<DOMMatrixReadOnly> rval =
      new DOMMatrixReadOnly(aGlobal.GetAsSupports(), matrixInit.mIs2D.Value());
  rval->SetDataFromMatrixInit(matrixInit);
  return rval.forget();
}


already_AddRefed<DOMMatrixReadOnly>
DOMMatrixReadOnly::FromFloat32Array(const GlobalObject& aGlobal, const Float32Array& aArray32, ErrorResult& aRv)
{
  aArray32.ComputeLengthAndData();

  const int length = aArray32.Length();
  const bool is2D = length == 6;
  RefPtr<DOMMatrixReadOnly> obj =
      new DOMMatrixReadOnly(aGlobal.GetAsSupports(), is2D);
  SetDataInMatrix(obj, aArray32.Data(), length, aRv);

  return obj.forget();
}

already_AddRefed<DOMMatrixReadOnly>
DOMMatrixReadOnly::FromFloat64Array(const GlobalObject& aGlobal, const Float64Array& aArray64, ErrorResult& aRv)
{
  aArray64.ComputeLengthAndData();

  const int length = aArray64.Length();
  const bool is2D = length == 6;
  RefPtr<DOMMatrixReadOnly> obj =
      new DOMMatrixReadOnly(aGlobal.GetAsSupports(), is2D);
  SetDataInMatrix(obj, aArray64.Data(), length, aRv);

  return obj.forget();
}

already_AddRefed<DOMMatrixReadOnly>
DOMMatrixReadOnly::Constructor(
  const GlobalObject& aGlobal,
  const Optional<StringOrUnrestrictedDoubleSequence>& aArg,
  ErrorResult& aRv)
{
  RefPtr<DOMMatrixReadOnly> rval = new DOMMatrixReadOnly(aGlobal.GetAsSupports());
  if (!aArg.WasPassed()) {
    return rval.forget();
  }

  const auto& arg = aArg.Value();
  if (arg.IsString()) {
    nsCOMPtr<nsPIDOMWindowInner> win = do_QueryInterface(aGlobal.GetAsSupports());
    if (!win) {
      aRv.ThrowTypeError<MSG_ILLEGAL_CONSTRUCTOR>();
      return nullptr;
    }
    rval->SetMatrixValue(arg.GetAsString(), aRv);
  } else {
    const auto& sequence = arg.GetAsUnrestrictedDoubleSequence();
    SetDataInMatrix(rval, sequence.Elements(), sequence.Length(), aRv);
  }

  return rval.forget();
}

already_AddRefed<DOMMatrixReadOnly>
DOMMatrixReadOnly::ReadStructuredClone(nsISupports* aParent, JSStructuredCloneReader* aReader)
{
  uint8_t is2D;

  if (!JS_ReadBytes(aReader, &is2D, 1)) {
    return nullptr;
  }

  RefPtr<DOMMatrixReadOnly> rval = new DOMMatrixReadOnly(aParent, is2D);

  if (!ReadStructuredCloneElements(aReader, rval)) {
    return nullptr;
  };

  return rval.forget();
}

already_AddRefed<DOMMatrix>
DOMMatrixReadOnly::Translate(double aTx,
                             double aTy,
                             double aTz) const
{
  RefPtr<DOMMatrix> retval = new DOMMatrix(mParent, *this);
  retval->TranslateSelf(aTx, aTy, aTz);

  return retval.forget();
}

already_AddRefed<DOMMatrix>
DOMMatrixReadOnly::Scale(double aScale,
                         double aOriginX,
                         double aOriginY) const
{
  RefPtr<DOMMatrix> retval = new DOMMatrix(mParent, *this);
  retval->ScaleSelf(aScale, aOriginX, aOriginY);

  return retval.forget();
}

already_AddRefed<DOMMatrix>
DOMMatrixReadOnly::Scale3d(double aScale,
                           double aOriginX,
                           double aOriginY,
                           double aOriginZ) const
{
  RefPtr<DOMMatrix> retval = new DOMMatrix(mParent, *this);
  retval->Scale3dSelf(aScale, aOriginX, aOriginY, aOriginZ);

  return retval.forget();
}

already_AddRefed<DOMMatrix>
DOMMatrixReadOnly::ScaleNonUniform(double aScaleX,
                                   double aScaleY,
                                   double aScaleZ,
                                   double aOriginX,
                                   double aOriginY,
                                   double aOriginZ) const
{
  RefPtr<DOMMatrix> retval = new DOMMatrix(mParent, *this);
  retval->ScaleNonUniformSelf(aScaleX, aScaleY, aScaleZ, aOriginX, aOriginY, aOriginZ);

  return retval.forget();
}

already_AddRefed<DOMMatrix>
DOMMatrixReadOnly::Rotate(double aRotX,
                          const Optional<double>& aRotY,
                          const Optional<double>& aRotZ) const {
  RefPtr<DOMMatrix> retval = new DOMMatrix(mParent, *this);
  retval->RotateSelf(aRotX, aRotY, aRotZ);

  return retval.forget();
}

already_AddRefed<DOMMatrix>
DOMMatrixReadOnly::RotateFromVector(double x,
                                    double y) const
{
  RefPtr<DOMMatrix> retval = new DOMMatrix(mParent, *this);
  retval->RotateFromVectorSelf(x, y);

  return retval.forget();
}

already_AddRefed<DOMMatrix>
DOMMatrixReadOnly::RotateAxisAngle(double aX,
                                   double aY,
                                   double aZ,
                                   double aAngle) const
{
  RefPtr<DOMMatrix> retval = new DOMMatrix(mParent, *this);
  retval->RotateAxisAngleSelf(aX, aY, aZ, aAngle);

  return retval.forget();
}

already_AddRefed<DOMMatrix>
DOMMatrixReadOnly::SkewX(double aSx) const
{
  RefPtr<DOMMatrix> retval = new DOMMatrix(mParent, *this);
  retval->SkewXSelf(aSx);

  return retval.forget();
}

already_AddRefed<DOMMatrix>
DOMMatrixReadOnly::SkewY(double aSy) const
{
  RefPtr<DOMMatrix> retval = new DOMMatrix(mParent, *this);
  retval->SkewYSelf(aSy);

  return retval.forget();
}

already_AddRefed<DOMMatrix>
DOMMatrixReadOnly::Multiply(const DOMMatrixInit& other, ErrorResult& aRv) const
{
  RefPtr<DOMMatrix> retval = new DOMMatrix(mParent, *this);
  retval->MultiplySelf(other, aRv);

  return retval.forget();
}

already_AddRefed<DOMMatrix>
DOMMatrixReadOnly::FlipX() const
{
  RefPtr<DOMMatrix> retval = new DOMMatrix(mParent, *this);
  if (mMatrix3D) {
    gfx::Matrix4x4 m;
    m._11 = -1;
    retval->mMatrix3D = new gfx::Matrix4x4(m * *mMatrix3D);
  } else {
    gfx::Matrix m;
    m._11 = -1;
    retval->mMatrix2D = new gfx::Matrix(mMatrix2D ? m * *mMatrix2D : m);
  }

  return retval.forget();
}

already_AddRefed<DOMMatrix>
DOMMatrixReadOnly::FlipY() const
{
  RefPtr<DOMMatrix> retval = new DOMMatrix(mParent, *this);
  if (mMatrix3D) {
    gfx::Matrix4x4 m;
    m._22 = -1;
    retval->mMatrix3D = new gfx::Matrix4x4(m * *mMatrix3D);
  } else {
    gfx::Matrix m;
    m._22 = -1;
    retval->mMatrix2D = new gfx::Matrix(mMatrix2D ? m * *mMatrix2D : m);
  }

  return retval.forget();
}

already_AddRefed<DOMMatrix>
DOMMatrixReadOnly::Inverse() const
{
  RefPtr<DOMMatrix> retval = new DOMMatrix(mParent, *this);
  retval->InvertSelf();

  return retval.forget();
}

bool
DOMMatrixReadOnly::Is2D() const
{
  return !mMatrix3D;
}

bool
DOMMatrixReadOnly::Identity() const
{
  if (mMatrix3D) {
    return mMatrix3D->IsIdentity();
  }

  return mMatrix2D->IsIdentity();
}

already_AddRefed<DOMPoint>
DOMMatrixReadOnly::TransformPoint(const DOMPointInit& point) const
{
  RefPtr<DOMPoint> retval = new DOMPoint(mParent);

  if (mMatrix3D) {
    gfx::Point4D transformedPoint;
    transformedPoint.x = point.mX;
    transformedPoint.y = point.mY;
    transformedPoint.z = point.mZ;
    transformedPoint.w = point.mW;

    transformedPoint = mMatrix3D->TransformPoint(transformedPoint);

    retval->SetX(transformedPoint.x);
    retval->SetY(transformedPoint.y);
    retval->SetZ(transformedPoint.z);
    retval->SetW(transformedPoint.w);
  } else if (point.mZ != 0 || point.mW != 1.0) {
    gfx::Matrix4x4 tempMatrix(gfx::Matrix4x4::From2D(*mMatrix2D));

    gfx::Point4D transformedPoint;
    transformedPoint.x = point.mX;
    transformedPoint.y = point.mY;
    transformedPoint.z = point.mZ;
    transformedPoint.w = point.mW;

    transformedPoint = tempMatrix.TransformPoint(transformedPoint);

    retval->SetX(transformedPoint.x);
    retval->SetY(transformedPoint.y);
    retval->SetZ(transformedPoint.z);
    retval->SetW(transformedPoint.w);
  } else {
    gfx::Point transformedPoint;
    transformedPoint.x = point.mX;
    transformedPoint.y = point.mY;

    transformedPoint = mMatrix2D->TransformPoint(transformedPoint);

    retval->SetX(transformedPoint.x);
    retval->SetY(transformedPoint.y);
    retval->SetZ(point.mZ);
    retval->SetW(point.mW);
  }
  return retval.forget();
}

template <typename T> void GetDataFromMatrix(const DOMMatrixReadOnly* aMatrix, T* aData)
{
  aData[0] = static_cast<T>(aMatrix->M11());
  aData[1] = static_cast<T>(aMatrix->M12());
  aData[2] = static_cast<T>(aMatrix->M13());
  aData[3] = static_cast<T>(aMatrix->M14());
  aData[4] = static_cast<T>(aMatrix->M21());
  aData[5] = static_cast<T>(aMatrix->M22());
  aData[6] = static_cast<T>(aMatrix->M23());
  aData[7] = static_cast<T>(aMatrix->M24());
  aData[8] = static_cast<T>(aMatrix->M31());
  aData[9] = static_cast<T>(aMatrix->M32());
  aData[10] = static_cast<T>(aMatrix->M33());
  aData[11] = static_cast<T>(aMatrix->M34());
  aData[12] = static_cast<T>(aMatrix->M41());
  aData[13] = static_cast<T>(aMatrix->M42());
  aData[14] = static_cast<T>(aMatrix->M43());
  aData[15] = static_cast<T>(aMatrix->M44());
}

void
DOMMatrixReadOnly::ToFloat32Array(JSContext* aCx, JS::MutableHandle<JSObject*> aResult, ErrorResult& aRv) const
{
  AutoTArray<float, 16> arr;
  arr.SetLength(16);
  GetDataFromMatrix(this, arr.Elements());
  JS::Rooted<JS::Value> value(aCx);
  if (!ToJSValue(aCx, TypedArrayCreator<Float32Array>(arr), &value)) {
    aRv.Throw(NS_ERROR_OUT_OF_MEMORY);
    return;
  }
  aResult.set(&value.toObject());
}

void
DOMMatrixReadOnly::ToFloat64Array(JSContext* aCx, JS::MutableHandle<JSObject*> aResult, ErrorResult& aRv) const
{
  AutoTArray<double, 16> arr;
  arr.SetLength(16);
  GetDataFromMatrix(this, arr.Elements());
  JS::Rooted<JS::Value> value(aCx);
  if (!ToJSValue(aCx, TypedArrayCreator<Float64Array>(arr), &value)) {
    aRv.Throw(NS_ERROR_OUT_OF_MEMORY);
    return;
  }
  aResult.set(&value.toObject());
}

void
DOMMatrixReadOnly::Stringify(nsAString& aResult)
{
  nsAutoString matrixStr;
  if (mMatrix3D) {
    matrixStr.AppendPrintf("matrix3d(%g, %g, %g, %g, %g, %g, %g, %g, %g, %g, %g, %g, %g, %g, %g, %g)",
      M11(), M12(), M13(), M14(),
      M21(), M22(), M23(), M24(),
      M31(), M32(), M33(), M34(),
      M41(), M42(), M43(), M44());
  } else {
    matrixStr.AppendPrintf("matrix(%g, %g, %g, %g, %g, %g)", A(), B(), C(), D(), E(), F());
  }

  aResult = matrixStr;
}

// https://drafts.fxtf.org/geometry/#structured-serialization
bool
DOMMatrixReadOnly::WriteStructuredClone(JSStructuredCloneWriter* aWriter) const
{
#define WriteFloatPair(f1, f2)                           \
  JS_WriteUint32Pair(aWriter, BitwiseCast<uint32_t>(f1), \
                     BitwiseCast<uint32_t>(f2))

  const uint8_t is2D = Is2D();

  if (!JS_WriteBytes(aWriter, &is2D, 1)) {
    return false;
  }

  if (is2D == 1) {
    return WriteFloatPair(mMatrix2D->_11, mMatrix2D->_12) &&
           WriteFloatPair(mMatrix2D->_21, mMatrix2D->_22) &&
           WriteFloatPair(mMatrix2D->_31, mMatrix2D->_32);
  }

  return WriteFloatPair(mMatrix3D->_11, mMatrix3D->_12) &&
         WriteFloatPair(mMatrix3D->_13, mMatrix3D->_14) &&
         WriteFloatPair(mMatrix3D->_21, mMatrix3D->_22) &&
         WriteFloatPair(mMatrix3D->_23, mMatrix3D->_24) &&
         WriteFloatPair(mMatrix3D->_31, mMatrix3D->_32) &&
         WriteFloatPair(mMatrix3D->_33, mMatrix3D->_34) &&
         WriteFloatPair(mMatrix3D->_41, mMatrix3D->_42) &&
         WriteFloatPair(mMatrix3D->_43, mMatrix3D->_44);
#undef WriteFloatPair
}

bool
DOMMatrixReadOnly::ReadStructuredCloneElements(JSStructuredCloneReader* aReader, DOMMatrixReadOnly* matrix)
{
  uint32_t high;
  uint32_t low;

#define ReadFloatPair(f1, f2)                     \
  if (!JS_ReadUint32Pair(aReader, &high, &low)) { \
    return false;                                 \
  }                                               \
  (*(f1) = BitwiseCast<float>(high));             \
  (*(f2) = BitwiseCast<float>(low));

  if (matrix->Is2D() == 1) {
    ReadFloatPair(&(matrix->mMatrix2D->_11), &(matrix->mMatrix2D->_12));
    ReadFloatPair(&(matrix->mMatrix2D->_21), &(matrix->mMatrix2D->_22));
    ReadFloatPair(&(matrix->mMatrix2D->_31), &(matrix->mMatrix2D->_32));
  } else {
    ReadFloatPair(&(matrix->mMatrix3D->_11), &(matrix->mMatrix3D->_12));
    ReadFloatPair(&(matrix->mMatrix3D->_13), &(matrix->mMatrix3D->_14));
    ReadFloatPair(&(matrix->mMatrix3D->_21), &(matrix->mMatrix3D->_22));
    ReadFloatPair(&(matrix->mMatrix3D->_23), &(matrix->mMatrix3D->_24));
    ReadFloatPair(&(matrix->mMatrix3D->_31), &(matrix->mMatrix3D->_32));
    ReadFloatPair(&(matrix->mMatrix3D->_33), &(matrix->mMatrix3D->_34));
    ReadFloatPair(&(matrix->mMatrix3D->_41), &(matrix->mMatrix3D->_42));
    ReadFloatPair(&(matrix->mMatrix3D->_43), &(matrix->mMatrix3D->_44));
  }

  return true;

#undef ReadFloatPair
}

already_AddRefed<DOMMatrix>
DOMMatrix::FromMatrix(nsISupports* aParent, const DOMMatrixInit& aMatrixInit, ErrorResult& aRv)
{
  DOMMatrixInit matrixInit(aMatrixInit);
  if (!ValidateAndFixupMatrixInit(matrixInit, aRv)) {
    return nullptr;
  };

  RefPtr<DOMMatrix> matrix = new DOMMatrix(aParent, matrixInit.mIs2D.Value());
  matrix->SetDataFromMatrixInit(matrixInit);
  return matrix.forget();
}

already_AddRefed<DOMMatrix>
DOMMatrix::FromMatrix(const GlobalObject& aGlobal, const DOMMatrixInit& aMatrixInit, ErrorResult& aRv)
{
  RefPtr<DOMMatrix> matrix =
      FromMatrix(aGlobal.GetAsSupports(), aMatrixInit, aRv);
  return matrix.forget();
}

already_AddRefed<DOMMatrix>
DOMMatrix::FromFloat32Array(const GlobalObject& aGlobal, const Float32Array& aArray32, ErrorResult& aRv)
{
  aArray32.ComputeLengthAndData();

  const int length = aArray32.Length();
  const bool is2D = length == 6;
  RefPtr<DOMMatrix> obj = new DOMMatrix(aGlobal.GetAsSupports(), is2D);
  SetDataInMatrix(obj, aArray32.Data(), length, aRv);

  return obj.forget();
}

already_AddRefed<DOMMatrix>
DOMMatrix::FromFloat64Array(const GlobalObject& aGlobal, const Float64Array& aArray64, ErrorResult& aRv)
{
  aArray64.ComputeLengthAndData();

  const int length = aArray64.Length();
  const bool is2D = length == 6;
  RefPtr<DOMMatrix> obj = new DOMMatrix(aGlobal.GetAsSupports(), is2D);
  SetDataInMatrix(obj, aArray64.Data(), length, aRv);

  return obj.forget();
}

already_AddRefed<DOMMatrix>
DOMMatrix::Constructor(const GlobalObject& aGlobal, ErrorResult& aRv)
{
  RefPtr<DOMMatrix> obj = new DOMMatrix(aGlobal.GetAsSupports());
  return obj.forget();
}

already_AddRefed<DOMMatrix>
DOMMatrix::Constructor(const GlobalObject& aGlobal, const nsAString& aTransformList, ErrorResult& aRv)
{
  nsCOMPtr<nsPIDOMWindowInner> win = do_QueryInterface(aGlobal.GetAsSupports());
  if (!win) {
    aRv.ThrowTypeError<MSG_ILLEGAL_CONSTRUCTOR>();
    return nullptr;
  }
  RefPtr<DOMMatrix> obj = new DOMMatrix(aGlobal.GetAsSupports());

  obj = obj->SetMatrixValue(aTransformList, aRv);
  return obj.forget();
}

already_AddRefed<DOMMatrix>
DOMMatrix::Constructor(const GlobalObject& aGlobal, const DOMMatrixReadOnly& aOther, ErrorResult& aRv)
{
  RefPtr<DOMMatrix> obj = new DOMMatrix(aGlobal.GetAsSupports(), aOther);
  return obj.forget();
}

template <typename T>
static void
SetDataInMatrix(DOMMatrixReadOnly* aMatrix, const T* aData, int aLength, ErrorResult& aRv)
{
  if (aLength == 16) {
    aMatrix->SetM11(aData[0]);
    aMatrix->SetM12(aData[1]);
    aMatrix->SetM13(aData[2]);
    aMatrix->SetM14(aData[3]);
    aMatrix->SetM21(aData[4]);
    aMatrix->SetM22(aData[5]);
    aMatrix->SetM23(aData[6]);
    aMatrix->SetM24(aData[7]);
    aMatrix->SetM31(aData[8]);
    aMatrix->SetM32(aData[9]);
    aMatrix->SetM33(aData[10]);
    aMatrix->SetM34(aData[11]);
    aMatrix->SetM41(aData[12]);
    aMatrix->SetM42(aData[13]);
    aMatrix->SetM43(aData[14]);
    aMatrix->SetM44(aData[15]);
  } else if (aLength == 6) {
    aMatrix->SetA(aData[0]);
    aMatrix->SetB(aData[1]);
    aMatrix->SetC(aData[2]);
    aMatrix->SetD(aData[3]);
    aMatrix->SetE(aData[4]);
    aMatrix->SetF(aData[5]);
  } else {
    nsAutoString lengthStr;
    lengthStr.AppendInt(aLength);
    aRv.ThrowTypeError<MSG_MATRIX_INIT_LENGTH_WRONG>(lengthStr);
  }
}

already_AddRefed<DOMMatrix>
DOMMatrix::Constructor(const GlobalObject& aGlobal, const Float32Array& aArray32, ErrorResult& aRv)
{
  return FromFloat32Array(aGlobal, aArray32, aRv);
}

already_AddRefed<DOMMatrix>
DOMMatrix::Constructor(const GlobalObject& aGlobal, const Float64Array& aArray64, ErrorResult& aRv)
{
  return FromFloat64Array(aGlobal, aArray64, aRv);
}

already_AddRefed<DOMMatrix>
DOMMatrix::Constructor(const GlobalObject& aGlobal, const Sequence<double>& aNumberSequence, ErrorResult& aRv)
{
  RefPtr<DOMMatrix> obj = new DOMMatrix(aGlobal.GetAsSupports());
  SetDataInMatrix(obj, aNumberSequence.Elements(), aNumberSequence.Length(), aRv);

  return obj.forget();
}

already_AddRefed<DOMMatrix>
DOMMatrix::ReadStructuredClone(nsISupports* aParent, JSStructuredCloneReader* aReader)
{
  uint8_t is2D;

  if (!JS_ReadBytes(aReader, &is2D, 1)) {
    return nullptr;
  }

  RefPtr<DOMMatrix> rval = new DOMMatrix(aParent, is2D);

  if (!ReadStructuredCloneElements(aReader, rval)) {
    return nullptr;
  };

  return rval.forget();
}

void
DOMMatrixReadOnly::Ensure3DMatrix()
{
  if (!mMatrix3D) {
    mMatrix3D = new gfx::Matrix4x4(gfx::Matrix4x4::From2D(*mMatrix2D));
    mMatrix2D = nullptr;
  }
}

DOMMatrix*
DOMMatrix::MultiplySelf(const DOMMatrixInit& aOtherInit, ErrorResult& aRv)
{
  RefPtr<DOMMatrix> other = FromMatrix(mParent, aOtherInit, aRv);
  if (other->Identity()) {
    return this;
  }

  if (other->Is2D()) {
    if (mMatrix3D) {
      *mMatrix3D = gfx::Matrix4x4::From2D(*other->mMatrix2D) * *mMatrix3D;
    } else {
      *mMatrix2D = *other->mMatrix2D * *mMatrix2D;
    }
  } else {
    Ensure3DMatrix();
    *mMatrix3D = *other->mMatrix3D * *mMatrix3D;
  }

  return this;
}

DOMMatrix*
DOMMatrix::PreMultiplySelf(const DOMMatrixInit& aOtherInit, ErrorResult& aRv)
{
  RefPtr<DOMMatrix> other = FromMatrix(mParent, aOtherInit, aRv);
  if (other->Identity()) {
    return this;
  }

  if (other->Is2D()) {
    if (mMatrix3D) {
      *mMatrix3D = *mMatrix3D * gfx::Matrix4x4::From2D(*other->mMatrix2D);
    } else {
      *mMatrix2D = *mMatrix2D * *other->mMatrix2D;
    }
  } else {
    Ensure3DMatrix();
    *mMatrix3D = *mMatrix3D * *other->mMatrix3D;
  }

  return this;
}

DOMMatrix*
DOMMatrix::TranslateSelf(double aTx,
                         double aTy,
                         double aTz)
{
  if (aTx == 0 && aTy == 0 && aTz == 0) {
    return this;
  }

  if (mMatrix3D || aTz != 0) {
    Ensure3DMatrix();
    mMatrix3D->PreTranslate(aTx, aTy, aTz);
  } else {
    mMatrix2D->PreTranslate(aTx, aTy);
  }

  return this;
}

DOMMatrix*
DOMMatrix::ScaleSelf(double aScale, double aOriginX, double aOriginY)
{
  ScaleNonUniformSelf(aScale, aScale, 1.0, aOriginX, aOriginY, 0);

  return this;
}

DOMMatrix*
DOMMatrix::Scale3dSelf(double aScale, double aOriginX,
                       double aOriginY, double aOriginZ)
{
  ScaleNonUniformSelf(aScale, aScale, aScale, aOriginX, aOriginY, aOriginZ);

  return this;
}

DOMMatrix*
DOMMatrix::ScaleNonUniformSelf(double aScaleX,
                               double aScaleY,
                               double aScaleZ,
                               double aOriginX,
                               double aOriginY,
                               double aOriginZ)
{
  if (aScaleX == 1.0 && aScaleY == 1.0 && aScaleZ == 1.0) {
    return this;
  }

  TranslateSelf(aOriginX, aOriginY, aOriginZ);

  if (mMatrix3D || aScaleZ != 1.0 || aOriginZ != 0) {
    Ensure3DMatrix();
    gfx::Matrix4x4 m;
    m._11 = aScaleX;
    m._22 = aScaleY;
    m._33 = aScaleZ;
    *mMatrix3D = m * *mMatrix3D;
  } else {
    gfx::Matrix m;
    m._11 = aScaleX;
    m._22 = aScaleY;
    *mMatrix2D = m * *mMatrix2D;
  }

  TranslateSelf(-aOriginX, -aOriginY, -aOriginZ);

  return this;
}

DOMMatrix*
DOMMatrix::RotateFromVectorSelf(double aX, double aY)
{
  if (aX == 0.0 || aY == 0.0) {
    return this;
  }

  const double angle = atan2(aY, aX);

  if (fmod(angle, 2 * M_PI) == 0) {
    return this;
  }

  if (mMatrix3D) {
    RotateAxisAngleSelf(0, 0, 1, angle / radPerDegree);
  } else {
    *mMatrix2D = mMatrix2D->PreRotate(angle);
  }

  return this;
}

DOMMatrix* DOMMatrix::RotateSelf(double aRotX, const Optional<double>& aRotY,
                                 const Optional<double>& aRotZ) {
  double rotY;
  double rotZ;
  if (!aRotY.WasPassed() && !aRotZ.WasPassed()) {
    rotZ = aRotX;
    aRotX = 0;
    rotY = 0;
  } else {
    rotY = aRotY.WasPassed() ? aRotY.Value() : 0;
    rotZ = aRotZ.WasPassed() ? aRotZ.Value() : 0;
  }

  if (aRotX != 0 || rotY != 0) {
    Ensure3DMatrix();
  }

  if (mMatrix3D) {
    if (fmod(rotZ, 360) != 0) {
      mMatrix3D->RotateZ(rotZ * radPerDegree);
    }
    if (fmod(rotY, 360) != 0) {
      mMatrix3D->RotateY(rotY * radPerDegree);
    }
    if (fmod(aRotX, 360) != 0) {
      mMatrix3D->RotateX(aRotX * radPerDegree);
    }
  } else if (fmod(rotZ, 360) != 0) {
    *mMatrix2D = mMatrix2D->PreRotate(rotZ * radPerDegree);
  }

  return this;
}

DOMMatrix*
DOMMatrix::RotateAxisAngleSelf(double aX, double aY,
                               double aZ, double aAngle)
{
  if (fmod(aAngle, 360) == 0) {
    return this;
  }

  aAngle *= radPerDegree;

  Ensure3DMatrix();
  gfx::Matrix4x4 m;
  m.SetRotateAxisAngle(aX, aY, aZ, aAngle);

  *mMatrix3D = m * *mMatrix3D;

  return this;
}

DOMMatrix*
DOMMatrix::SkewXSelf(double aSx)
{
  if (fmod(aSx, 360) == 0) {
    return this;
  }

  if (mMatrix3D) {
    gfx::Matrix4x4 m;
    m._21 = tan(aSx * radPerDegree);
    *mMatrix3D = m * *mMatrix3D;
  } else {
    gfx::Matrix m;
    m._21 = tan(aSx * radPerDegree);
    *mMatrix2D = m * *mMatrix2D;
  }

  return this;
}

DOMMatrix*
DOMMatrix::SkewYSelf(double aSy)
{
  if (fmod(aSy, 360) == 0) {
    return this;
  }

  if (mMatrix3D) {
    gfx::Matrix4x4 m;
    m._12 = tan(aSy * radPerDegree);
    *mMatrix3D = m * *mMatrix3D;
  } else {
    gfx::Matrix m;
    m._12 = tan(aSy * radPerDegree);
    *mMatrix2D = m * *mMatrix2D;
  }

  return this;
}

DOMMatrix*
DOMMatrix::InvertSelf()
{
  if (mMatrix3D) {
    if (!mMatrix3D->Invert()) {
      mMatrix3D->SetNAN();
    }
  } else if (!mMatrix2D->Invert()) {
    mMatrix2D = nullptr;

    mMatrix3D = new gfx::Matrix4x4();
    mMatrix3D->SetNAN();
  }

  return this;
}

DOMMatrixReadOnly*
DOMMatrixReadOnly::SetMatrixValue(const nsAString& aTransformList, ErrorResult& aRv)
{
  SVGTransformListParser parser(aTransformList);
  if (!parser.Parse()) {
    aRv.Throw(NS_ERROR_DOM_SYNTAX_ERR);
  } else {
    mMatrix3D = nullptr;
    mMatrix2D = new gfx::Matrix();
    gfxMatrix result;
    const nsTArray<nsSVGTransform>& mItems = parser.GetTransformList();

    for (uint32_t i = 0; i < mItems.Length(); ++i) {
      result.PreMultiply(mItems[i].GetMatrix());
    }

    SetA(result._11);
    SetB(result._12);
    SetC(result._21);
    SetD(result._22);
    SetE(result._31);
    SetF(result._32);
  }

  return this;
}

DOMMatrix*
DOMMatrix::SetMatrixValue(const nsAString& aTransformList, ErrorResult& aRv)
{
  DOMMatrixReadOnly::SetMatrixValue(aTransformList, aRv);
  return this;
}

JSObject*
DOMMatrix::WrapObject(JSContext* aCx, JS::Handle<JSObject*> aGivenProto)
{
  return DOMMatrixBinding::Wrap(aCx, this, aGivenProto);
}

} // namespace dom
} // namespace mozilla
