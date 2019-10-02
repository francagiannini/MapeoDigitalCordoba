<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>
<qgis maxScale="1000" minScale="1e+8" version="3.2.2-Bonn" hasScaleBasedVisibilityFlag="0">
  <pipe>
    <rasterrenderer classificationMin="0" band="1" type="singlebandpseudocolor" opacity="1" classificationMax="24" alphaBand="-1">
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
          <item label="0" value="0" color="#ffffff" alpha="255"/>
          <item label="Monte" value="1" color="#009800" alpha="255"/>
          <item label="Arbustales y matorrales" value="2" color="#65d92b" alpha="255"/>
          <item label="Pastizal natural" value="3" color="#c94ed0" alpha="255"/>
          <item label="Pastizal natural con rocas o suelo desnudo" value="4" color="#fb90e1" alpha="255"/>
          <item label="Rocas" value="5" color="#9d9d9d" alpha="255"/>
          <item label="Suelo desnudo" value="6" color="#ebebeb" alpha="255"/>
          <item label="Salina" value="7" color="#e2e2e2" alpha="255"/>
          <item label="Cuerpos de agua" value="8" color="#5970a1" alpha="255"/>
          <item label="Zonas anegables" value="9" color="#acdee9" alpha="255"/>
          <item label="Cursos de agua" value="10" color="#0000ff" alpha="255"/>
          <item label="Zona urbana consolidada" value="11" color="#cd3411" alpha="255"/>
          <item label="Zona urbana en proceso de consolidación" value="12" color="#e56549" alpha="255"/>
          <item label="Zona urbana sin consolidar" value="13" color="#ffab98" alpha="255"/>
          <item label="Infraestructura vial" value="14" color="#000000" alpha="255"/>
          <item label="Actividades invernales" value="15" color="#f1f1a6" alpha="255"/>
          <item label="Actividades estivales" value="16" color="#d8d894" alpha="255"/>
          <item label="Actividades anuales (doble ciclo)" value="17" color="#bebe82" alpha="255"/>
          <item label="Sin actividad significativa" value="18" color="#86865c" alpha="255"/>
          <item label="Cultivos anuales irrigados" value="19" color="#f7fa41" alpha="255"/>
          <item label="Pasturas implantadas" value="20" color="#07eb98" alpha="255"/>
          <item label="Pasturas naturales manejadas" value="21" color="#adde8f" alpha="255"/>
          <item label="Plantaciones forestales maderables" value="22" color="#84371d" alpha="255"/>
          <item label="Plantaciones perennes (frutales) de secano" value="23" color="#a8583d" alpha="255"/>
          <item label="Plantaciones perennes (frutales) irrigadas" value="24" color="#b07a67" alpha="255"/>
        </colorrampshader>
      </rastershader>
    </rasterrenderer>
    <brightnesscontrast brightness="-1" contrast="0"/>
    <huesaturation grayscaleMode="0" colorizeOn="0" colorizeBlue="128" colorizeStrength="100" colorizeGreen="128" saturation="0" colorizeRed="255"/>
    <rasterresampler maxOversampling="2"/>
  </pipe>
  <blendMode>0</blendMode>
</qgis>
