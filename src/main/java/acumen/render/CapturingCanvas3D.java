package acumen.render;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.*;

import javax.imageio.ImageIO;
//import javax.imageio.spi.ImageReaderWriterSpi;
import javax.media.j3d.*;
import javax.vecmath.*;
//import com.sun.image.codec.jpeg.*;

public class CapturingCanvas3D extends Canvas3D  {

	private static final long serialVersionUID = -3852342962102411898L;
	private int postSwapCount_;

	public CapturingCanvas3D(GraphicsConfiguration gc) {
		super(gc);
		postSwapCount_ = 0;
	}

	public void postSwap() {
		System.out.println("Writing image");
		GraphicsContext3D  ctx = getGraphicsContext3D();
		// The raster components need all be set!
		Raster ras = new Raster(
				new Point3f(-1.0f,-1.0f,-1.0f),
				Raster.RASTER_COLOR,
				0,0,
				1280,800,
				new ImageComponent2D(
						ImageComponent.FORMAT_RGB,
						new BufferedImage(1280,800,
								BufferedImage.TYPE_INT_RGB)),
								null);

		ctx.readRaster(ras);

		// Now strip out the image info
		BufferedImage img = ras.getImage().getImage();

		// write that to disk....
		try {
			FileOutputStream out = new FileOutputStream("/tmp/Capture" + String.format("%05d", postSwapCount_) +".bmp");
			//JPEGImageEncoder encoder = JPEGCodec.createJPEGEncoder(out);
			//JPEGEncodeParam param = encoder.getDefaultJPEGEncodeParam(img);
			//param.setQuality(0.9f,false); // 90% qualith JPEG
			//encoder.setJPEGEncodeParam(param);
			//encoder.encode(img);
			ImageIO.write(img, "bmp", out);
			out.close();
		} catch ( IOException e ) {
			System.out.println("I/O exception!");
		}
		postSwapCount_++;
	}
}