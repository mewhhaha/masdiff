#include <SDL3/SDL.h>
#include <SDL3/SDL_gpu.h>

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

typedef struct Vertex {
  float x, y;
  float u, v;
} Vertex;

typedef struct FragUniforms {
  float textColor[4];
  float params[4]; /* params.x = pxRange, params.y = 0 (MSDF) / 1 (MTSDF), params.z = debug */
  float atlasSize[2];
  float screenSize[2];
} FragUniforms;

static void die(const char *msg) {
  SDL_LogError(SDL_LOG_CATEGORY_APPLICATION, "%s: %s", msg, SDL_GetError());
  exit(1);
}

static int file_exists(const char *path) {
  FILE *f = fopen(path, "rb");
  if (f) {
    fclose(f);
    return 1;
  }
  return 0;
}

static const char *resolve_out_dir(void) {
  if (file_exists("out/meta_msdf.txt")) {
    return "out";
  }
  if (file_exists("out/meta_mtsdf.txt")) {
    return "out";
  }
  if (file_exists("out/meta.txt")) {
    return "out";
  }
  if (file_exists("examples/sdl_gpu_wesl/out/meta_msdf.txt")) {
    return "examples/sdl_gpu_wesl/out";
  }
  if (file_exists("examples/sdl_gpu_wesl/out/meta_mtsdf.txt")) {
    return "examples/sdl_gpu_wesl/out";
  }
  if (file_exists("examples/sdl_gpu_wesl/out/meta.txt")) {
    return "examples/sdl_gpu_wesl/out";
  }
  return NULL;
}

static void make_path(char *dst, size_t cap, const char *dir, const char *file) {
  SDL_snprintf(dst, cap, "%s/%s", dir, file);
}

static int write_tga(const char *path, int w, int h, const void *data, SDL_GPUTextureFormat fmt) {
  const Uint8 *bytes = (const Uint8 *) data;
  bool is_bgra = fmt == SDL_GPU_TEXTUREFORMAT_B8G8R8A8_UNORM ||
                 fmt == SDL_GPU_TEXTUREFORMAT_B8G8R8A8_UNORM_SRGB;
  bool is_rgba = fmt == SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM ||
                 fmt == SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM_SRGB;

  if (!is_bgra && !is_rgba) {
    return -1;
  }

  FILE *f = fopen(path, "wb");
  if (!f) {
    return -1;
  }

  Uint8 header[18] = {0};
  header[2] = 2; /* uncompressed truecolor */
  header[12] = (Uint8) (w & 0xFF);
  header[13] = (Uint8) ((w >> 8) & 0xFF);
  header[14] = (Uint8) (h & 0xFF);
  header[15] = (Uint8) ((h >> 8) & 0xFF);
  header[16] = 32; /* bpp */
  header[17] = 0x20; /* top-left origin */
  fwrite(header, 1, sizeof(header), f);

  if (is_bgra) {
    fwrite(bytes, 1, (size_t) w * (size_t) h * 4u, f);
  } else {
    for (int y = 0; y < h; y++) {
      const Uint8 *row = bytes + (size_t) y * (size_t) w * 4u;
      for (int x = 0; x < w; x++) {
        const Uint8 *px = row + (size_t) x * 4u;
        Uint8 out[4] = { px[2], px[1], px[0], px[3] }; /* RGBA -> BGRA */
        fwrite(out, 1, 4, f);
      }
    }
  }

  fclose(f);
  return 0;
}

typedef struct MetaInfo {
  int atlasW;
  int atlasH;
  int screenW;
  int screenH;
  float pxRange;
  Uint32 vertexCount;
} MetaInfo;

typedef struct DemoOutput {
  const char *label;
  float formatFlag;
  MetaInfo meta;
  void *atlasData;
  size_t atlasSize;
  void *vertData;
  size_t vertSize;
  SDL_GPUTexture *atlasTex;
  SDL_GPUBuffer *vertexBuffer;
  SDL_GPUTransferBuffer *texUpload;
  SDL_GPUTransferBuffer *vtxUpload;
} DemoOutput;

static int read_u32_le(const Uint8 *data, size_t len, size_t *off, Uint32 *out) {
  if (*off + 4 > len) {
    return 0;
  }
  const Uint8 *p = data + *off;
  *out = (Uint32) p[0] | ((Uint32) p[1] << 8) | ((Uint32) p[2] << 16) | ((Uint32) p[3] << 24);
  *off += 4;
  return 1;
}

static int read_f32_le(const Uint8 *data, size_t len, size_t *off, float *out) {
  Uint32 raw = 0;
  if (!read_u32_le(data, len, off, &raw)) {
    return 0;
  }
  union { Uint32 u; float f; } v;
  v.u = raw;
  *out = v.f;
  return 1;
}

static void *read_all_stdin(size_t *outSize) {
  size_t cap = 65536;
  size_t size = 0;
  Uint8 *buf = (Uint8 *) SDL_malloc(cap);
  if (!buf) {
    return NULL;
  }
  for (;;) {
    if (size == cap) {
      cap *= 2;
      Uint8 *next = (Uint8 *) SDL_realloc(buf, cap);
      if (!next) {
        SDL_free(buf);
        return NULL;
      }
      buf = next;
    }
    size_t n = fread(buf + size, 1, cap - size, stdin);
    size += n;
    if (n == 0) {
      break;
    }
  }
  if (ferror(stdin)) {
    SDL_free(buf);
    return NULL;
  }
  *outSize = size;
  return buf;
}

static int find_magic(const Uint8 *data, size_t len, const char *magic, size_t magicLen, size_t *outPos) {
  if (magicLen == 0 || len < magicLen) {
    return 0;
  }
  for (size_t i = 0; i + magicLen <= len; i++) {
    if (memcmp(data + i, magic, magicLen) == 0) {
      *outPos = i;
      return 1;
    }
  }
  return 0;
}

static int load_blob_from_stdin(DemoOutput **outOutputs, int *outCount,
                                void **outVs, size_t *outVsSize,
                                void **outFs, size_t *outFsSize) {
  const char *magic = "MSDFBLB1";
  size_t blobSize = 0;
  Uint8 *blob = (Uint8 *) read_all_stdin(&blobSize);
  if (!blob || blobSize == 0) {
    SDL_free(blob);
    return -1;
  }
  size_t magicPos = 0;
  if (!find_magic(blob, blobSize, magic, 8, &magicPos)) {
    SDL_free(blob);
    return -1;
  }
  size_t off = magicPos + 8;
  Uint32 version = 0;
  if (!read_u32_le(blob, blobSize, &off, &version) || version != 1) {
    SDL_free(blob);
    return -1;
  }
  Uint32 vsSize = 0, fsSize = 0;
  if (!read_u32_le(blob, blobSize, &off, &vsSize) ||
      !read_u32_le(blob, blobSize, &off, &fsSize)) {
    SDL_free(blob);
    return -1;
  }
  if (off + vsSize + fsSize > blobSize) {
    SDL_free(blob);
    return -1;
  }
  void *vsCode = SDL_malloc(vsSize);
  void *fsCode = SDL_malloc(fsSize);
  if (!vsCode || !fsCode) {
    SDL_free(vsCode);
    SDL_free(fsCode);
    SDL_free(blob);
    return -1;
  }
  memcpy(vsCode, blob + off, vsSize);
  off += vsSize;
  memcpy(fsCode, blob + off, fsSize);
  off += fsSize;

  Uint32 count = 0;
  if (!read_u32_le(blob, blobSize, &off, &count) || count == 0) {
    SDL_free(vsCode);
    SDL_free(fsCode);
    SDL_free(blob);
    return -1;
  }
  DemoOutput *outputs = (DemoOutput *) SDL_calloc(count, sizeof(DemoOutput));
  if (!outputs) {
    SDL_free(vsCode);
    SDL_free(fsCode);
    SDL_free(blob);
    return -1;
  }
  for (Uint32 i = 0; i < count; i++) {
    Uint32 labelLen = 0;
    if (!read_u32_le(blob, blobSize, &off, &labelLen) || off + labelLen > blobSize) {
      SDL_free(outputs);
      SDL_free(vsCode);
      SDL_free(fsCode);
      SDL_free(blob);
      return -1;
    }
    char *label = (char *) SDL_malloc(labelLen + 1);
    if (!label) {
      SDL_free(outputs);
      SDL_free(vsCode);
      SDL_free(fsCode);
      SDL_free(blob);
      return -1;
    }
    memcpy(label, blob + off, labelLen);
    label[labelLen] = '\0';
    off += labelLen;

    float formatFlag = 0.0f;
    if (!read_f32_le(blob, blobSize, &off, &formatFlag)) {
      SDL_free(label);
      SDL_free(outputs);
      SDL_free(vsCode);
      SDL_free(fsCode);
      SDL_free(blob);
      return -1;
    }

    Uint32 atlasW = 0, atlasH = 0, pixelSize = 0, screenW = 0, screenH = 0;
    Uint32 vertexCount = 0, atlasSize = 0, vertSize = 0;
    float pxRange = 0.0f;
    if (!read_u32_le(blob, blobSize, &off, &atlasW) ||
        !read_u32_le(blob, blobSize, &off, &atlasH) ||
        !read_u32_le(blob, blobSize, &off, &pixelSize) ||
        !read_u32_le(blob, blobSize, &off, &screenW) ||
        !read_u32_le(blob, blobSize, &off, &screenH) ||
        !read_f32_le(blob, blobSize, &off, &pxRange) ||
        !read_u32_le(blob, blobSize, &off, &vertexCount) ||
        !read_u32_le(blob, blobSize, &off, &atlasSize) ||
        !read_u32_le(blob, blobSize, &off, &vertSize)) {
      SDL_free(label);
      SDL_free(outputs);
      SDL_free(vsCode);
      SDL_free(fsCode);
      SDL_free(blob);
      return -1;
    }
    (void) pixelSize;
    (void) vertexCount;
    if (atlasSize != (Uint32) atlasW * (Uint32) atlasH * 4u) {
      SDL_free(label);
      SDL_free(outputs);
      SDL_free(vsCode);
      SDL_free(fsCode);
      SDL_free(blob);
      return -1;
    }
    if (vertSize % sizeof(Vertex) != 0) {
      SDL_free(label);
      SDL_free(outputs);
      SDL_free(vsCode);
      SDL_free(fsCode);
      SDL_free(blob);
      return -1;
    }
    if (off + atlasSize + vertSize > blobSize) {
      SDL_free(label);
      SDL_free(outputs);
      SDL_free(vsCode);
      SDL_free(fsCode);
      SDL_free(blob);
      return -1;
    }
    void *atlasData = NULL;
    void *vertData = NULL;
    if (atlasSize == 0 && vertSize == 0) {
      /* empty output, keep buffers null */
    } else if (atlasSize > 0 && vertSize > 0) {
      atlasData = SDL_malloc(atlasSize);
      vertData = SDL_malloc(vertSize);
      if (!atlasData || !vertData) {
        SDL_free(atlasData);
        SDL_free(vertData);
        SDL_free(label);
        SDL_free(outputs);
        SDL_free(vsCode);
        SDL_free(fsCode);
        SDL_free(blob);
        return -1;
      }
      memcpy(atlasData, blob + off, atlasSize);
      off += atlasSize;
      memcpy(vertData, blob + off, vertSize);
      off += vertSize;
    } else {
      SDL_free(label);
      SDL_free(outputs);
      SDL_free(vsCode);
      SDL_free(fsCode);
      SDL_free(blob);
      return -1;
    }

    outputs[i].label = label;
    outputs[i].formatFlag = formatFlag;
    outputs[i].meta.atlasW = (int) atlasW;
    outputs[i].meta.atlasH = (int) atlasH;
    outputs[i].meta.screenW = (int) screenW;
    outputs[i].meta.screenH = (int) screenH;
    outputs[i].meta.pxRange = pxRange;
    outputs[i].meta.vertexCount = (Uint32) (vertSize / sizeof(Vertex));
    outputs[i].atlasData = atlasData;
    outputs[i].atlasSize = atlasSize;
    outputs[i].vertData = vertData;
    outputs[i].vertSize = vertSize;
  }
  SDL_free(blob);
  *outOutputs = outputs;
  *outCount = (int) count;
  *outVs = vsCode;
  *outVsSize = vsSize;
  *outFs = fsCode;
  *outFsSize = fsSize;
  return 0;
}

static int load_meta(const char *path, MetaInfo *meta) {
  FILE *f = fopen(path, "r");
  if (!f) {
    return -1;
  }
  char key[64];
  double val = 0.0;
  while (fscanf(f, "%63s %lf", key, &val) == 2) {
    if (strcmp(key, "atlasWidth") == 0) {
      meta->atlasW = (int) val;
    } else if (strcmp(key, "atlasHeight") == 0) {
      meta->atlasH = (int) val;
    } else if (strcmp(key, "screenWidth") == 0) {
      meta->screenW = (int) val;
    } else if (strcmp(key, "screenHeight") == 0) {
      meta->screenH = (int) val;
    } else if (strcmp(key, "pxRange") == 0) {
      meta->pxRange = (float) val;
    } else if (strcmp(key, "vertexCount") == 0) {
      meta->vertexCount = (Uint32) val;
    }
  }
  fclose(f);
  return (meta->atlasW > 0 && meta->atlasH > 0) ? 0 : -1;
}

int main(int argc, char **argv) {
  bool inMemory = SDL_getenv("SDL_MSDF_IN_MEMORY") != NULL;
  for (int i = 1; i < argc; i++) {
    if (strcmp(argv[i], "--stdin") == 0 || strcmp(argv[i], "--blob") == 0) {
      inMemory = true;
    }
  }

  bool forceHeadless = SDL_getenv("SDL_MSDF_HEADLESS") != NULL;
  if (forceHeadless) {
    SDL_setenv_unsafe("SDL_VIDEODRIVER", "dummy", 0);
    SDL_setenv_unsafe("SDL_HIDAPI_UDEV", "0", 0);
    SDL_setenv_unsafe("SDL_JOYSTICK_HIDAPI", "0", 0);
    SDL_setenv_unsafe("SDL_AUTO_UPDATE_JOYSTICKS", "0", 0);
  }
  int screenshotDelayMs = 0;
  const char *delayEnv = SDL_getenv("SDL_MSDF_SCREENSHOT_DELAY_MS");
  if (delayEnv && delayEnv[0] != '\0') {
    screenshotDelayMs = atoi(delayEnv);
  }
  float debugMode = 0.f;
  const char *debugEnv = SDL_getenv("SDL_MSDF_DEBUG_VIEW");
  bool debugGrid = SDL_getenv("SDL_MSDF_DEBUG_GRID") != NULL;
  if (debugEnv && debugEnv[0] != '\0') {
    if (strcmp(debugEnv, "alpha") == 0) {
      debugMode = 1.f;
    } else if (strcmp(debugEnv, "r") == 0) {
      debugMode = 2.f;
    } else if (strcmp(debugEnv, "g") == 0) {
      debugMode = 3.f;
    } else if (strcmp(debugEnv, "b") == 0) {
      debugMode = 4.f;
    } else if (strcmp(debugEnv, "median") == 0) {
      debugMode = 5.f;
    } else if (strcmp(debugEnv, "split") == 0) {
      debugMode = 6.f;
    } else if (strcmp(debugEnv, "fill") == 0) {
      debugMode = 7.f;
    } else {
      SDL_LogWarn(SDL_LOG_CATEGORY_APPLICATION, "unknown SDL_MSDF_DEBUG_VIEW=%s", debugEnv);
    }
  } else if (SDL_getenv("SDL_MSDF_ALPHA_ONLY") != NULL) {
    debugMode = 1.f;
  }
  bool forceNearest = SDL_getenv("SDL_MSDF_NEAREST") != NULL;

  if (SDL_Init(SDL_INIT_VIDEO) < 0) {
    die("SDL_Init");
  }

  SDL_Window *window = NULL;
  bool headless = forceHeadless;
  if (!headless) {
    window = SDL_CreateWindow("masdiff SDL_gpu", 1280, 720, SDL_WINDOW_RESIZABLE);
    if (!window) {
      SDL_LogWarn(SDL_LOG_CATEGORY_APPLICATION, "SDL_CreateWindow failed: %s (falling back to headless)", SDL_GetError());
      headless = true;
    }
  }

  int driverCount = SDL_GetNumGPUDrivers();
  if (driverCount > 0) {
    SDL_Log("SDL GPU drivers:");
    for (int i = 0; i < driverCount; i++) {
      const char *name = SDL_GetGPUDriver(i);
      SDL_Log("  - %s", name ? name : "(null)");
    }
  }

  const char *backendEnv = SDL_getenv("SDL_GPU_DRIVER");
  const char *backend = (backendEnv && backendEnv[0] != '\0')
                          ? backendEnv
                          : (headless ? "vulkan" : NULL);
  SDL_GPUDevice *gpu = SDL_CreateGPUDevice(SDL_GPU_SHADERFORMAT_SPIRV, true, backend);
  if (!gpu && backend) {
    SDL_LogWarn(SDL_LOG_CATEGORY_APPLICATION, "SDL_CreateGPUDevice(%s) failed: %s (retrying default)",
                backend, SDL_GetError());
    gpu = SDL_CreateGPUDevice(SDL_GPU_SHADERFORMAT_SPIRV, true, NULL);
  }
  if (!gpu) {
    die("SDL_CreateGPUDevice");
  }
  if (window) {
    if (!SDL_ClaimWindowForGPUDevice(gpu, window)) {
      die("SDL_ClaimWindowForGPUDevice");
    }
  }

  const char *outDir = NULL;
  DemoOutput *outputs = NULL;
  int outputTotal = 0;
  int outputCount = 0;
  void *vsCode = NULL;
  void *fsCode = NULL;
  size_t vsSize = 0;
  size_t fsSize = 0;
  bool outputsOwned = false;
  int screenW = 1280;
  int screenH = 720;
  char path[256];

  if (inMemory) {
    if (load_blob_from_stdin(&outputs, &outputCount, &vsCode, &vsSize, &fsCode, &fsSize) != 0) {
      die("load in-memory blob");
    }
    outputTotal = outputCount;
    outputsOwned = true;
    const char *outEnv = SDL_getenv("SDL_MSDF_OUT_DIR");
    outDir = (outEnv && outEnv[0] != '\0') ? outEnv : ".";
    if (outputCount > 0) {
      if (outputs[0].meta.screenW > 0) {
        screenW = outputs[0].meta.screenW;
      }
      if (outputs[0].meta.screenH > 0) {
        screenH = outputs[0].meta.screenH;
      }
    }
    for (int i = 0; i < outputTotal; i++) {
      if (outputs[i].meta.pxRange <= 0.f) {
        outputs[i].meta.pxRange = 4.f;
      }
    }
  } else {
    outDir = resolve_out_dir();
    if (!outDir) {
      die("could not locate out/meta_msdf.txt or out/meta.txt");
    }

    bool pairWeights = false;
    const char *pairEnv = SDL_getenv("SDL_MSDF_VARIATION_PAIR");
    if (pairEnv && pairEnv[0] != '\0') {
      pairWeights = true;
    }
    const char *pairEnv2 = SDL_getenv("SDL_MSDF_PAIR_WEIGHTS");
    if (pairEnv2 && pairEnv2[0] != '\0') {
      pairWeights = true;
    }
    outputTotal = pairWeights ? 2 : 1;
    outputs = (DemoOutput *) SDL_calloc(outputTotal, sizeof(DemoOutput));
    if (!outputs) {
      die("alloc outputs");
    }
    outputsOwned = true;
    outputs[0].label = pairWeights ? "w400" : "mtsdf";
    outputs[0].formatFlag = 1.f;
    if (outputTotal > 1) {
      outputs[1].label = "w900";
      outputs[1].formatFlag = 1.f;
    }

    for (int i = 0; i < outputTotal; i++) {
      DemoOutput *out = &outputs[i];
      MetaInfo meta = {0};
      bool metaOk = false;
      if (out->label) {
        SDL_snprintf(path, sizeof(path), "%s/meta_%s.txt", outDir, out->label);
        if (load_meta(path, &meta) == 0) {
          metaOk = true;
        }
      }
      if (!metaOk && i == 0) {
        make_path(path, sizeof(path), outDir, "meta.txt");
        if (load_meta(path, &meta) == 0) {
          metaOk = true;
        }
      }
      if (!metaOk) {
        continue;
      }
      if (meta.pxRange <= 0.f) {
        meta.pxRange = 4.f;
      }
      out->meta = meta;
      if (outputCount == 0) {
        screenW = meta.screenW > 0 ? meta.screenW : screenW;
        screenH = meta.screenH > 0 ? meta.screenH : screenH;
      }

      size_t atlasSize = 0;
      void *atlasData = NULL;
      if (out->label) {
        SDL_snprintf(path, sizeof(path), "%s/atlas_%s.rgba", outDir, out->label);
        atlasData = SDL_LoadFile(path, &atlasSize);
      }
      if (!atlasData && i == 0) {
        make_path(path, sizeof(path), outDir, "atlas.rgba");
        atlasData = SDL_LoadFile(path, &atlasSize);
      }
      if (!atlasData) {
        SDL_LogError(SDL_LOG_CATEGORY_APPLICATION, "load atlas for %s failed", out->label);
        continue;
      }
      if (atlasSize != (size_t) meta.atlasW * (size_t) meta.atlasH * 4u) {
        SDL_LogError(SDL_LOG_CATEGORY_APPLICATION, "atlas size mismatch for %s", out->label);
        SDL_free(atlasData);
        continue;
      }
      if (atlasSize == 0) {
        SDL_LogError(SDL_LOG_CATEGORY_APPLICATION, "atlas empty for %s", out->label);
        SDL_free(atlasData);
        continue;
      }
      out->atlasData = atlasData;
      out->atlasSize = atlasSize;

      size_t vertSize = 0;
      void *vertData = NULL;
      if (out->label) {
        SDL_snprintf(path, sizeof(path), "%s/vertices_%s.bin", outDir, out->label);
        vertData = SDL_LoadFile(path, &vertSize);
      }
      if (!vertData && i == 0) {
        make_path(path, sizeof(path), outDir, "vertices.bin");
        vertData = SDL_LoadFile(path, &vertSize);
      }
      if (!vertData) {
        SDL_LogError(SDL_LOG_CATEGORY_APPLICATION, "load vertices for %s failed", out->label);
        SDL_free(atlasData);
        out->atlasData = NULL;
        continue;
      }
      if (vertSize % sizeof(Vertex) != 0) {
        SDL_LogError(SDL_LOG_CATEGORY_APPLICATION, "vertex size mismatch for %s", out->label);
        SDL_free(atlasData);
        SDL_free(vertData);
        out->atlasData = NULL;
        continue;
      }
      if (vertSize == 0) {
        SDL_LogError(SDL_LOG_CATEGORY_APPLICATION, "vertex data empty for %s", out->label);
        SDL_free(atlasData);
        SDL_free(vertData);
        out->atlasData = NULL;
        continue;
      }
      out->vertData = vertData;
      out->vertSize = vertSize;
      out->meta.vertexCount = (Uint32) (vertSize / sizeof(Vertex));
      outputCount++;
    }

    if (outputCount == 0) {
      die("no demo outputs found (missing meta/atlas/vertices)");
    }

    make_path(path, sizeof(path), outDir, "msdf.vert.spv");
    vsCode = SDL_LoadFile(path, &vsSize);
    if (!vsCode) {
      die("load msdf.vert.spv");
    }
    make_path(path, sizeof(path), outDir, "msdf.frag.spv");
    fsCode = SDL_LoadFile(path, &fsSize);
    if (!fsCode) {
      die("load msdf.frag.spv");
    }
  }
  if (outputTotal == 0) {
    die("no demo outputs found");
  }

  bool wantScreenshot = SDL_getenv("SDL_MSDF_SCREENSHOT") != NULL;
  bool captured = false;
  SDL_GPUTransferBuffer *readback = NULL;
  Uint32 readbackSize = 0;
  SDL_GPUTextureFormat swapFmt = window ? SDL_GetGPUSwapchainTextureFormat(gpu, window)
                                        : SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM;
  if (headless) {
    wantScreenshot = true;
  }

  SDL_GPUShader *vs = SDL_CreateGPUShader(gpu, &(SDL_GPUShaderCreateInfo){
    .code = (const Uint8 *) vsCode,
    .code_size = vsSize,
    .entrypoint = "vs_main",
    .format = SDL_GPU_SHADERFORMAT_SPIRV,
    .stage = SDL_GPU_SHADERSTAGE_VERTEX,
    .num_samplers = 0,
    .num_storage_textures = 0,
    .num_storage_buffers = 0,
    .num_uniform_buffers = 0
  });
  if (!vs) {
    die("create vertex shader");
  }

  SDL_GPUShader *fs = SDL_CreateGPUShader(gpu, &(SDL_GPUShaderCreateInfo){
    .code = (const Uint8 *) fsCode,
    .code_size = fsSize,
    .entrypoint = "fs_main",
    .format = SDL_GPU_SHADERFORMAT_SPIRV,
    .stage = SDL_GPU_SHADERSTAGE_FRAGMENT,
    .num_samplers = 1,
    .num_storage_textures = 0,
    .num_storage_buffers = 0,
    .num_uniform_buffers = 1
  });
  if (!fs) {
    die("create fragment shader");
  }

  SDL_GPUVertexBufferDescription vb = {
    .slot = 0,
    .pitch = sizeof(Vertex),
    .input_rate = SDL_GPU_VERTEXINPUTRATE_VERTEX
  };

  SDL_GPUVertexAttribute attrs[2] = {
    { .location = 0, .buffer_slot = 0, .format = SDL_GPU_VERTEXELEMENTFORMAT_FLOAT2, .offset = 0 },
    { .location = 1, .buffer_slot = 0, .format = SDL_GPU_VERTEXELEMENTFORMAT_FLOAT2, .offset = sizeof(float) * 2 }
  };

  SDL_GPUColorTargetBlendState blend = {
    .src_color_blendfactor = SDL_GPU_BLENDFACTOR_SRC_ALPHA,
    .dst_color_blendfactor = SDL_GPU_BLENDFACTOR_ONE_MINUS_SRC_ALPHA,
    .color_blend_op = SDL_GPU_BLENDOP_ADD,
    .src_alpha_blendfactor = SDL_GPU_BLENDFACTOR_ONE,
    .dst_alpha_blendfactor = SDL_GPU_BLENDFACTOR_ONE_MINUS_SRC_ALPHA,
    .alpha_blend_op = SDL_GPU_BLENDOP_ADD,
    .color_write_mask = SDL_GPU_COLORCOMPONENT_R | SDL_GPU_COLORCOMPONENT_G |
                        SDL_GPU_COLORCOMPONENT_B | SDL_GPU_COLORCOMPONENT_A,
    .enable_blend = true,
    .enable_color_write_mask = true
  };

  SDL_GPUTextureFormat targetFmt = swapFmt;
  SDL_GPUTexture *renderTarget = NULL;
  Uint32 offW = (Uint32) screenW;
  Uint32 offH = (Uint32) screenH;

  if (headless) {
    targetFmt = SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM;
  }

  SDL_GPUColorTargetDescription colorDesc = { 0 };
  colorDesc.format = targetFmt;
  colorDesc.blend_state = blend;

  SDL_GPURasterizerState rs = { 0 };
  rs.fill_mode = SDL_GPU_FILLMODE_FILL;
  rs.cull_mode = SDL_GPU_CULLMODE_NONE;
  rs.front_face = SDL_GPU_FRONTFACE_COUNTER_CLOCKWISE;
  rs.enable_depth_clip = true;

  SDL_GPUMultisampleState ms = { 0 };
  ms.sample_count = SDL_GPU_SAMPLECOUNT_1;

  SDL_GPUDepthStencilState ds = { 0 };
  ds.enable_depth_test = false;
  ds.enable_depth_write = false;

  SDL_GPUGraphicsPipelineCreateInfo pipeInfo = { 0 };
  pipeInfo.vertex_shader = vs;
  pipeInfo.fragment_shader = fs;
  pipeInfo.vertex_input_state.vertex_buffer_descriptions = &vb;
  pipeInfo.vertex_input_state.num_vertex_buffers = 1;
  pipeInfo.vertex_input_state.vertex_attributes = attrs;
  pipeInfo.vertex_input_state.num_vertex_attributes = 2;
  pipeInfo.primitive_type = SDL_GPU_PRIMITIVETYPE_TRIANGLELIST;
  pipeInfo.rasterizer_state = rs;
  pipeInfo.multisample_state = ms;
  pipeInfo.depth_stencil_state = ds;
  pipeInfo.target_info.color_target_descriptions = &colorDesc;
  pipeInfo.target_info.num_color_targets = 1;
  pipeInfo.target_info.depth_stencil_format = SDL_GPU_TEXTUREFORMAT_INVALID;
  pipeInfo.target_info.has_depth_stencil_target = false;

  SDL_GPUGraphicsPipeline *pipeline = SDL_CreateGPUGraphicsPipeline(gpu, &pipeInfo);
  if (!pipeline) {
    die("create pipeline");
  }

  if (headless) {
    renderTarget = SDL_CreateGPUTexture(gpu, &(SDL_GPUTextureCreateInfo){
      .type = SDL_GPU_TEXTURETYPE_2D,
      .format = targetFmt,
      .usage = SDL_GPU_TEXTUREUSAGE_COLOR_TARGET,
      .width = offW,
      .height = offH,
      .layer_count_or_depth = 1,
      .num_levels = 1,
      .sample_count = SDL_GPU_SAMPLECOUNT_1
    });
    if (!renderTarget) {
      die("create offscreen render target");
    }
  }

  SDL_GPUSampler *sampler = SDL_CreateGPUSampler(gpu, &(SDL_GPUSamplerCreateInfo){
    .min_filter = forceNearest ? SDL_GPU_FILTER_NEAREST : SDL_GPU_FILTER_LINEAR,
    .mag_filter = forceNearest ? SDL_GPU_FILTER_NEAREST : SDL_GPU_FILTER_LINEAR,
    .mipmap_mode = SDL_GPU_SAMPLERMIPMAPMODE_NEAREST,
    .address_mode_u = SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE,
    .address_mode_v = SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE,
    .address_mode_w = SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE
  });
  if (!sampler) {
    die("create sampler");
  }
  for (int i = 0; i < outputTotal; i++) {
    DemoOutput *out = &outputs[i];
    if (!out->atlasData || !out->vertData || out->atlasSize == 0 || out->vertSize == 0) {
      continue;
    }
    out->atlasTex = SDL_CreateGPUTexture(gpu, &(SDL_GPUTextureCreateInfo){
      .type = SDL_GPU_TEXTURETYPE_2D,
      .format = SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM,
      .usage = SDL_GPU_TEXTUREUSAGE_SAMPLER,
      .width = (Uint32) out->meta.atlasW,
      .height = (Uint32) out->meta.atlasH,
      .layer_count_or_depth = 1,
      .num_levels = 1,
      .sample_count = SDL_GPU_SAMPLECOUNT_1
    });
    if (!out->atlasTex) {
      die("create atlas texture");
    }
    out->vertexBuffer = SDL_CreateGPUBuffer(gpu, &(SDL_GPUBufferCreateInfo){
      .usage = SDL_GPU_BUFFERUSAGE_VERTEX,
      .size = (Uint32) out->vertSize
    });
    if (!out->vertexBuffer) {
      die("create vertex buffer");
    }
    out->texUpload = SDL_CreateGPUTransferBuffer(gpu, &(SDL_GPUTransferBufferCreateInfo){
      .usage = SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD,
      .size = (Uint32) out->atlasSize
    });
    out->vtxUpload = SDL_CreateGPUTransferBuffer(gpu, &(SDL_GPUTransferBufferCreateInfo){
      .usage = SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD,
      .size = (Uint32) out->vertSize
    });
    if (!out->texUpload || !out->vtxUpload) {
      die("create transfer buffers");
    }
  }

  bool uploaded = false;
  bool running = true;
  while (running) {
    SDL_Event ev;
    if (window) {
      while (SDL_PollEvent(&ev)) {
        if (ev.type == SDL_EVENT_QUIT) {
          running = false;
        }
      }
    }

    SDL_GPUCommandBuffer *cmd = SDL_AcquireGPUCommandBuffer(gpu);
    if (!cmd) {
      die("acquire command buffer");
    }

    if (!uploaded) {
      SDL_GPUCopyPass *copy = SDL_BeginGPUCopyPass(cmd);
      for (int i = 0; i < outputTotal; i++) {
        DemoOutput *out = &outputs[i];
        if (!out->atlasData || !out->vertData) {
          continue;
        }
        void *mappedTex = SDL_MapGPUTransferBuffer(gpu, out->texUpload, false);
        SDL_memcpy(mappedTex, out->atlasData, out->atlasSize);
        SDL_UnmapGPUTransferBuffer(gpu, out->texUpload);

        void *mappedVtx = SDL_MapGPUTransferBuffer(gpu, out->vtxUpload, false);
        SDL_memcpy(mappedVtx, out->vertData, out->vertSize);
        SDL_UnmapGPUTransferBuffer(gpu, out->vtxUpload);

        SDL_UploadToGPUTexture(copy,
          &(SDL_GPUTextureTransferInfo){ out->texUpload, 0, (Uint32) out->meta.atlasW, (Uint32) out->meta.atlasH },
          &(SDL_GPUTextureRegion){ out->atlasTex, 0, 0, 0, 0, 0, (Uint32) out->meta.atlasW, (Uint32) out->meta.atlasH, 1 },
          false);
        SDL_UploadToGPUBuffer(copy,
          &(SDL_GPUTransferBufferLocation){ out->vtxUpload, 0 },
          &(SDL_GPUBufferRegion){ out->vertexBuffer, 0, (Uint32) out->vertSize },
          false);
      }
      SDL_EndGPUCopyPass(copy);
      uploaded = true;
    }

    SDL_GPUTexture *swapchainTex = NULL;
    Uint32 swapW = 0, swapH = 0;
    if (window) {
      if (!SDL_WaitAndAcquireGPUSwapchainTexture(cmd, window, &swapchainTex, &swapW, &swapH)) {
        SDL_SubmitGPUCommandBuffer(cmd);
        continue;
      }
    } else {
      swapchainTex = renderTarget;
      swapW = offW;
      swapH = offH;
    }

    if (wantScreenshot && readback == NULL) {
      readbackSize = swapW * swapH * 4u;
      readback = SDL_CreateGPUTransferBuffer(gpu, &(SDL_GPUTransferBufferCreateInfo){
        .usage = SDL_GPU_TRANSFERBUFFERUSAGE_DOWNLOAD,
        .size = readbackSize
      });
      if (!readback) {
        die("create readback buffer");
      }
    }

    SDL_GPUColorTargetInfo colorTarget = { 0 };
    colorTarget.texture = swapchainTex;
    colorTarget.load_op = SDL_GPU_LOADOP_CLEAR;
    colorTarget.store_op = SDL_GPU_STOREOP_STORE;
    colorTarget.clear_color = (SDL_FColor){ 0.f, 0.f, 0.f, 1.f };

    SDL_GPURenderPass *pass = SDL_BeginGPURenderPass(cmd, &colorTarget, 1, NULL);
    SDL_BindGPUGraphicsPipeline(pass, pipeline);

    for (int i = 0; i < outputTotal; i++) {
      DemoOutput *out = &outputs[i];
      if (!out->atlasTex || !out->vertexBuffer) {
        continue;
      }
      SDL_GPUBufferBinding vbind = { out->vertexBuffer, 0 };
      SDL_BindGPUVertexBuffers(pass, 0, &vbind, 1);

      SDL_GPUTextureSamplerBinding sbind = { out->atlasTex, sampler };
      SDL_BindGPUFragmentSamplers(pass, 0, &sbind, 1);

      if (debugGrid) {
        const int cols = 4;
        const int rows = 2;
        const float modes[8] = { 1.f, 7.f, 6.f, 5.f, 2.f, 3.f, 4.f, 0.f };
        const int cellW = (int) swapW / cols;
        const int cellH = (int) swapH / rows;
        for (int m = 0; m < 8; m++) {
          const int col = m % cols;
          const int row = m / cols;
          SDL_GPUViewport vp = { (float) (col * cellW), (float) (row * cellH), (float) cellW, (float) cellH, 0.f, 1.f };
          SDL_SetGPUViewport(pass, &vp);
          SDL_Rect scissor = { col * cellW, row * cellH, cellW, cellH };
          SDL_SetGPUScissor(pass, &scissor);
          FragUniforms fu = {
            { 1.f, 1.f, 1.f, 1.f },
            { out->meta.pxRange, out->formatFlag, modes[m], 0.f },
            { (float) out->meta.atlasW, (float) out->meta.atlasH },
            { (float) cellW, (float) cellH }
          };
          SDL_PushGPUFragmentUniformData(cmd, 0, &fu, (Uint32) sizeof(fu));
          SDL_DrawGPUPrimitives(pass, out->meta.vertexCount, 1, 0, 0);
        }
      } else {
        SDL_GPUViewport vp = { 0.f, 0.f, (float) swapW, (float) swapH, 0.f, 1.f };
        SDL_SetGPUViewport(pass, &vp);
        SDL_Rect scissor = { 0, 0, (int) swapW, (int) swapH };
        SDL_SetGPUScissor(pass, &scissor);
        FragUniforms fu = {
          { 1.f, 1.f, 1.f, 1.f },
          { out->meta.pxRange, out->formatFlag, debugMode, 0.f },
          { (float) out->meta.atlasW, (float) out->meta.atlasH },
          { (float) swapW, (float) swapH }
        };
        SDL_PushGPUFragmentUniformData(cmd, 0, &fu, (Uint32) sizeof(fu));
        SDL_DrawGPUPrimitives(pass, out->meta.vertexCount, 1, 0, 0);
      }
    }
    SDL_EndGPURenderPass(pass);

    SDL_GPUFence *fence = NULL;
    if (wantScreenshot && !captured) {
      SDL_GPUCopyPass *copy2 = SDL_BeginGPUCopyPass(cmd);
      SDL_DownloadFromGPUTexture(copy2,
        &(SDL_GPUTextureRegion){ swapchainTex, 0, 0, 0, 0, 0, swapW, swapH, 1 },
        &(SDL_GPUTextureTransferInfo){ readback, 0, 0, 0 });
      SDL_EndGPUCopyPass(copy2);
      fence = SDL_SubmitGPUCommandBufferAndAcquireFence(cmd);
    } else {
      SDL_SubmitGPUCommandBuffer(cmd);
    }

    if (fence) {
      SDL_GPUFence *fences[1] = { fence };
      SDL_WaitForGPUFences(gpu, true, fences, 1);
      SDL_ReleaseGPUFence(gpu, fence);
      void *mapped = SDL_MapGPUTransferBuffer(gpu, readback, false);
      if (mapped) {
        char shotPath[256];
        make_path(shotPath, sizeof(shotPath), outDir, "screenshot.tga");
        if (write_tga(shotPath, (int) swapW, (int) swapH, mapped, swapFmt) == 0) {
          SDL_Log("wrote %s", shotPath);
        } else {
          SDL_LogError(SDL_LOG_CATEGORY_APPLICATION, "failed to write screenshot");
        }
        SDL_UnmapGPUTransferBuffer(gpu, readback);
      }
      if (screenshotDelayMs > 0 && window) {
        SDL_Delay((Uint32) screenshotDelayMs);
      }
      captured = true;
      running = false;
    }
    if (!window && !wantScreenshot) {
      running = false;
    }
  }

  SDL_free(vsCode);
  SDL_free(fsCode);

  if (readback) {
    SDL_ReleaseGPUTransferBuffer(gpu, readback);
  }
  for (int i = 0; i < outputTotal; i++) {
    DemoOutput *out = &outputs[i];
    if (out->atlasData) {
      SDL_free(out->atlasData);
    }
    if (out->vertData) {
      SDL_free(out->vertData);
    }
    if (inMemory && out->label) {
      SDL_free((void *) out->label);
    }
    if (out->texUpload) {
      SDL_ReleaseGPUTransferBuffer(gpu, out->texUpload);
    }
    if (out->vtxUpload) {
      SDL_ReleaseGPUTransferBuffer(gpu, out->vtxUpload);
    }
    if (out->vertexBuffer) {
      SDL_ReleaseGPUBuffer(gpu, out->vertexBuffer);
    }
    if (out->atlasTex) {
      SDL_ReleaseGPUTexture(gpu, out->atlasTex);
    }
  }
  if (outputsOwned) {
    SDL_free(outputs);
  }
  if (renderTarget) {
    SDL_ReleaseGPUTexture(gpu, renderTarget);
  }
  SDL_ReleaseGPUSampler(gpu, sampler);
  SDL_ReleaseGPUGraphicsPipeline(gpu, pipeline);
  SDL_ReleaseGPUShader(gpu, vs);
  SDL_ReleaseGPUShader(gpu, fs);
  SDL_DestroyGPUDevice(gpu);
  if (window) {
    SDL_DestroyWindow(window);
  }
  SDL_Quit();
  return 0;
}
