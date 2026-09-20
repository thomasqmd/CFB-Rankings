<script>
// Image zoom on click for presentations and reports (pure JS, no jQuery)
document.addEventListener("DOMContentLoaded", function () {
  const zoomDiv = document.createElement("div");
  zoomDiv.className = "zoomDiv";
  zoomDiv.style.cssText = "position: fixed; top: 50%; left: 50%; transform: translate(-50%, -50%); z-index: 9999; max-height: 90vh; max-width: 90vw; opacity: 0; pointer-events: none; transition: opacity 0.2s ease; box-shadow: 0 0 40px rgba(0,0,0,0.5); border-radius: 6px; overflow: hidden; background: transparent;";

  const zoomImg = document.createElement("img");
  zoomImg.className = "zoomImg";
  zoomImg.style.cssText = "width: 100%; height: auto; max-height: 90vh; object-fit: contain; cursor: zoom-out; display: block; border-radius: 6px;";
  zoomDiv.appendChild(zoomImg);
  document.body.appendChild(zoomDiv);

  const backdrop = document.createElement("div");
  backdrop.className = "zoomBackdrop";
  backdrop.style.cssText = "position: fixed; top: 0; left: 0; width: 100vw; height: 100vh; background: rgba(0,0,0,0.6); z-index: 9998; opacity: 0; pointer-events: none; transition: opacity 0.2s ease;";
  document.body.appendChild(backdrop);

  function closeZoom() {
    zoomDiv.style.opacity = "0";
    zoomDiv.style.pointerEvents = "none";
    backdrop.style.opacity = "0";
    backdrop.style.pointerEvents = "none";
  }

  zoomDiv.addEventListener("click", closeZoom);
  backdrop.addEventListener("click", closeZoom);

  document.querySelectorAll(".reveal .slides img, main.content img, .quarto-figure img, .cell-output-display img").forEach(function (img) {
    if (img.classList.contains("zoomImg") || img.closest(".slide-logo") || img.closest(".brand-doc-header")) return;
    img.style.cursor = "zoom-in";
    img.addEventListener("click", function (e) {
      e.stopPropagation();
      zoomImg.src = img.src;
      zoomDiv.style.opacity = "1";
      zoomDiv.style.pointerEvents = "auto";
      backdrop.style.opacity = "1";
      backdrop.style.pointerEvents = "auto";
    });
  });
});
</script>
