<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>
<qgis maxScale="0" minScale="1e+8" version="3.2.2-Bonn" hasScaleBasedVisibilityFlag="0">
  <pipe>
    <rasterrenderer classificationMin="0" band="1" type="singlebandpseudocolor" opacity="1" classificationMax="9" alphaBand="-1">
      <rasterTransparency/>
      <minMaxOrigin>
        <limits>None</limits>
        <extent>WholeRaster</extent>
        <statAccuracy>Exact</statAccuracy>
        <cumulativeCutLower>0.02</cumulativeCutLower>
        <cumulativeCutUpper>0.98</cumulativeCutUpper>
        <stdDevFactor>2</stdDevFactor>
      </minMaxOrigin>
      <rastershader>
        <colorrampshader colorRampType="INTERPOLATED" classificationMode="1" clip="0">
          <colorramp type="gradient" name="[source]">
            <prop v="215,25,28,255" k="color1"/>
            <prop v="43,131,186,255" k="color2"/>
            <prop v="0" k="discrete"/>
            <prop v="gradient" k="rampType"/>
            <prop v="0.25;253,174,97,255:0.5;255,255,191,255:0.75;171,221,164,255" k="stops"/>
          </colorramp>
          <item label="0" value="0" color="#000000" alpha="0"/>
          <item label="Bosques" value="1" color="#397f20" alpha="255"/>
          <item label="Arbustales y matorrales" value="2" color="#e5e875" alpha="255"/>
          <item label="Pastizales" value="3" color="#c4be3e" alpha="255"/>
          <item label="Suelo desnudo (Rocas, arenales y salinas)" value="4" color="#cf99fc" alpha="255"/>
          <item label="Cuerpos de agua y áreas anegables" value="5" color="#4163f8" alpha="255"/>
          <item label="Infraestructura y asentamientos humanos" value="6" color="#818181" alpha="255"/>
          <item label="Cultivos anuales y pasturas manejadas" value="7" color="#e0c291" alpha="255"/>
          <item label="Plantaciones forestales (maderables)" value="8" color="#87c803" alpha="255"/>
          <item label="Plantaciones perennes (frutales)" value="9" color="#fb94f8" alpha="255"/>
        </colorrampshader>
      </rastershader>
    </rasterrenderer>
    <brightnesscontrast brightness="0" contrast="0"/>
    <huesaturation grayscaleMode="0" colorizeOn="0" colorizeBlue="128" colorizeStrength="100" colorizeGreen="128" saturation="0" colorizeRed="255"/>
    <rasterresampler maxOversampling="2"/>
  </pipe>
  <blendMode>0</blendMode>
</qgis>
