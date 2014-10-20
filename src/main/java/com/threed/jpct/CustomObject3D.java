package com.threed.jpct;

import java.lang.reflect.Field;

public class CustomObject3D {

  public static void partialBuild(Object3D o, Boolean isText) {
    try {
      o.hasBeenBuild = true;

      if (!isText) {
        if (!(o.isMainWorld)) {
          o.calcCenter();
          o.calcBoundingBox();
        } else {
          o.reorderSectors(1);
          o.myWorld.portals.calcAABoundingBox(o);
        }
      }

      if (!(o.objMesh.normalsCalculated)) {
        o.calcNormals();
      }
      o.recreateTextureCoords();

      GLSLShader localGLSLShader = null;

      Field renderHookField = o.getClass().getDeclaredField("renderHook");
      renderHookField.setAccessible(true);
      IRenderHook renderHook = (IRenderHook) renderHookField.get(o);
      if ((renderHook != null) && (renderHook instanceof GLSLShader)) {
        localGLSLShader = (GLSLShader) renderHook;
      }
      if ((localGLSLShader != null) && (localGLSLShader.needsTangents())
          && (!(o.objMesh.tangentsCalculated)))
        o.objMesh.calculateTangentVectors(o.objVectors);
    } catch (Exception localException) {
      localException.printStackTrace();
      Logger.log(
          "Couldn't build() object (" + o.name + "): "
              + localException.getClass().getName() + "/"
              + localException.getMessage(), 0);
    }
  }

}
