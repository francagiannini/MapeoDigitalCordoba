<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>
<qgis maxScale="1000" minScale="1e+08" styleCategories="AllStyleCategories" version="3.6.0-Noosa" hasScaleBasedVisibilityFlag="0">
  <flags>
    <Identifiable>1</Identifiable>
    <Removable>1</Removable>
    <Searchable>1</Searchable>
  </flags>
  <customproperties>
    <property key="WMSBackgroundLayer" value="false"/>
    <property key="WMSPublishDataSourceUrl" value="false"/>
    <property key="embeddedWidgets/count" value="0"/>
    <property key="identify/format" value="Value"/>
  </customproperties>
  <pipe>
    <rasterrenderer type="singlebandpseudocolor" opacity="1" alphaBand="-1" classificationMin="0" classificationMax="27" band="1">
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
        <colorrampshader classificationMode="1" clip="0" colorRampType="INTERPOLATED">
          <colorramp type="gradient" name="[source]">
            <prop k="color1" v="215,25,28,255"/>
            <prop k="color2" v="43,131,186,255"/>
            <prop k="discrete" v="0"/>
            <prop k="rampType" v="gradient"/>
            <prop k="stops" v="0.25;253,174,97,255:0.5;255,255,191,255:0.75;171,221,164,255"/>
          </colorramp>
          <item color="#ffffff" value="0" alpha="255" label="0"/>
          <item color="#009800" value="1" alpha="255" label="Monte"/>
          <item color="#65d92b" value="2" alpha="255" label="Arbustales y matorrales"/>
          <item color="#c94ed0" value="3" alpha="255" label="Pastizal natural"/>
          <item color="#f560ff" value="4" alpha="255" label="Pastizal natural con rocas o suelo desnudo"/>
          <item color="#9d9d9d" value="5" alpha="255" label="Rocas"/>
          <item color="#ebebeb" value="6" alpha="255" label="Suelo desnudo"/>
          <item color="#e2e2e2" value="7" alpha="255" label="Salina"/>
          <item color="#5970a1" value="8" alpha="255" label="Cuerpos de agua"/>
          <item color="#acdee9" value="9" alpha="255" label="Zonas anegables"/>
          <item color="#0000ff" value="10" alpha="255" label="Cursos de agua"/>
          <item color="#cd1010" value="11" alpha="255" label="Zona urbana consolidada"/>
          <item color="#e54949" value="12" alpha="255" label="Zona urbana en proceso de consolidación"/>
          <item color="#ff9898" value="13" alpha="255" label="Zona urbana sin consolidar"/>
          <item color="#000000" value="14" alpha="255" label="Infraestructura vial"/>
          <item color="#e4ca00" value="15" alpha="255" label="Trigo"/>
          <item color="#e97f02" value="16" alpha="255" label="Maíz"/>
          <item color="#a8ae00" value="17" alpha="255" label="Soja"/>
          <item color="#bd5a50" value="18" alpha="255" label="Maní"/>
          <item color="#76274f" value="19" alpha="255" label="Sorgo"/>
          <item color="#cdcd73" value="20" alpha="255" label="Trigo-Maíz de segunda"/>
          <item color="#888c3c" value="21" alpha="255" label="Trigo-Soja de segunda"/>
          <item color="#fffd2e" value="22" alpha="255" label="Cultivos anuales irrigados"/>
          <item color="#07eb98" value="23" alpha="255" label="Pasturas implantadas"/>
          <item color="#b8ec98" value="24" alpha="255" label="Pasturas naturales manejadas"/>
          <item color="#84371d" value="25" alpha="255" label="Plantaciones forestales maderables"/>
          <item color="#a8583d" value="26" alpha="255" label="Plantaciones perennes (frutales) de secano"/>
          <item color="#b07a67" value="27" alpha="255" label="Plantaciones perennes (frutales) irrigadas"/>
        </colorrampshader>
      </rastershader>
    </rasterrenderer>
    <brightnesscontrast contrast="0" brightness="-1"/>
    <huesaturation colorizeBlue="128" colorizeOn="0" grayscaleMode="0" saturation="0" colorizeGreen="128" colorizeStrength="100" colorizeRed="255"/>
    <rasterresampler maxOversampling="2"/>
  </pipe>
  <blendMode>0</blendMode>
</qgis>
